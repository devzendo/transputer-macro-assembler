/*
 * Copyright (C) 2008-2023 Matt Gumbley, DevZendo.org http://devzendo.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.devzendo.tma.parser

import java.io
import java.io.File

import org.devzendo.tma.{Includer, SourceLocation}
import org.devzendo.tma.ast.AST.{Label, MacroArgument, MacroName, MacroParameterName}
import org.devzendo.tma.ast._
import org.log4s.Logger

import scala.util.matching.Regex
import scala.util.parsing.combinator._

class AssemblyParser(val debugParser: Boolean, val showParserOutput: Boolean, val macroManager: MacroManager, val includer: Includer) {

    val logger: Logger = org.log4s.getLogger

    var processorToParse: Option[Processor] = None

    @throws(classOf[AssemblyParserException])
    def parse(line: String, location: SourceLocation, inMacroExpansion: Boolean = false): List[Line] = {

        def sanitizedInput = nullToEmpty(line).trim()

        def logParserOutput(rLines: List[Line]): Unit = {
            logger.info(s"${rLines.size} lines output...")
            for (rL <- rLines) {
                val sb = new StringBuilder()
                sb.append("AST ")
                sb.append(rL.location.lineNumber)
                sb.append("|")
                for (label <- rL.label) {
                    sb.append("LBL ")
                    sb.append(label)
                    sb.append(":")
                }
                for (stmt <- rL.stmt) {
                    sb.append(stmt)
                }
                sb.append("|")
                logger.info(sb.toString())
                logger.info("")
            }
        }


        if (debugParser) {
            logger.debug(s"parsing IME $inMacroExpansion ${location.lineNumber}|$sanitizedInput|")
        }
        if (showParserOutput) {
            val lineType = if (inMacroExpansion) "IME" else "TXT"
            logger.info(s"$lineType ${location.lineNumber}|$sanitizedInput|")
        }
        if (location.lineNumber < 1) {
            throw new AssemblyParserException(location, "Line numbers must be positive")
        }

        if (sanitizedInput.length > 0) {
            val parser = if (macroManager.isInMacroBody) new MacroBodyCombinatorParser else new StatementCombinatorParser(inMacroExpansion)
            parser.setSourceLocation(location)
            parser.setDebugParser(debugParser)

            val parserOutput = parser.parseProgram(sanitizedInput)
            parserOutput match {
                case parser.Success(r, _) =>
                    val rLines = r.asInstanceOf[List[Line]] // unsure why r is not right type
                    if (debugParser) {
                        logger.debug("returning " + rLines)
                    }

                    if (showParserOutput) {
                        logParserOutput(rLines)
                    }
                    rLines

                case parser.NoSuccess(x, _) =>
                    logger.debug(s"${location.lineNumber}: $sanitizedInput") // mostly a useless, hard to understand error...
                    throw new AssemblyParserException(location, "Unknown statement '" + sanitizedInput + "'")
            }
        } else {
            val line = Line(location, sanitizedInput, None, None)
            List(line)
        }
    }

    private def nullToEmpty(input: String): String = {
        if (input == null) "" else input
    }

    private class MacroBodyCombinatorParser extends DiagnosableParser with LineParser {
        def line: Parser[List[Line]] = macroEnd | macroStart | macroBody

        def macroEnd: Parser[List[Line]] =
            """(?i)ENDM""".r ^^ {
                _ =>
                    if (debugParser) logger.debug("in endm")
                    macroManager.endMacro()
                    List(Line(location, text, None, Some(MacroEnd())))
            }

        def macroStart: Parser[List[Line]] = (
            ident ~ macroWord ~ repsep(ident, ",")
            ) ^^ {
                _ => throw new AssemblyParserException(location, "Macro definitions cannot be nested")
        }

        def macroWord: Parser[String] =
            """(?i)MACRO""".r ^^ ( _ => "MACRO" )

        def macroBody: Parser[List[Line]] =
        """.*""".r ^^ {
            x: String =>
                if (debugParser) logger.debug("in macroBody")
                macroManager.addMacroLine(x)
                List(Line(location, text, None, Some(MacroBody(x))))
        }
    }

    private trait NoInstructionParser extends Parsers {
        def noInstructionsAllowed: Parser[Statement] = failure("No instruction parser available")
    }

    private class StatementCombinatorParser(inMacroExpansion: Boolean) extends ExpressionParser with DiagnosableParser with LineParser
        with TransputerInstructionParser with NoInstructionParser {

        def line: Parser[List[Line]] =  macroInvocationLine | statementLine

        def statementLine: Parser[List[Line]] = (
            opt(label) ~ opt(statement) ~ opt(comment)
          ) ^^ {
            case optLabel ~ optStatement ~ optComment =>
                if (debugParser) logger.debug("in statementLine, inMacroExpansion=" + inMacroExpansion + ", optComment=" + optComment)
                val returnedText = removeDoubleSemicolonCommentsInMacroExpansion(optComment)
                if (debugParser) logger.debug("in statementLine after ;;-removal, text=|" + returnedText + "|")
                List(Line(location, returnedText, optLabel, optStatement))
        }

        private def removeDoubleSemicolonCommentsInMacroExpansion(optComment: Option[Comment]): String = {
            val returnedText = (inMacroExpansion, optComment) match {
                case (true, Some(DoubleComment(comment: String))) => text.substring(0, text.length - comment.length).trim
                case _ => text // already trimmed
            }
            returnedText
        }

        def replaceFirstLabel(maybeLabel: Option[Label], lines: List[Line]): List[Line] = {
            if (maybeLabel.isEmpty || lines.isEmpty) {
                lines
            } else {
                lines.head.copy(label = maybeLabel) :: lines.tail
            }
        }

        def macroInvocationLine: Parser[List[Line]] = (
          opt(label) ~ macroInvocation <~ opt(comment)
          ) ^^ {
            case optLabel ~ macroInvocation =>
                if (debugParser) {
                    logger.debug("in macroInvocationLine, macroInvocation is " + macroInvocation)
                    macroInvocation.args.foreach( (ma: MacroArgument) => logger.debug(s"macro argument |$ma|"))
                }

                // Ensure that any label in the original macro invocation is not passed through to the code generator.
                // That would cause it to set the label twice (which it can't). Any label will be passed through to
                // the first expanded parsed line.
                val macroInvocationLine = Line(location, text, None, Some(macroInvocation))

                val expansion = macroManager.expandMacro(macroInvocation.name, macroInvocation.args)
                if (debugParser) expansion.foreach( (f: String) => logger.debug("expanded macro: |" + f + "|"))
                val parsedExpansions = expansion.flatMap((str: String) => AssemblyParser.this.parse(str, location, inMacroExpansion = true))
                // Ensure that any label in the macro invocation is set in the first expanded parsed line
                val parsedExpansionsWithInvocationLabel = replaceFirstLabel(optLabel, parsedExpansions)
                if (debugParser) parsedExpansionsWithInvocationLabel.foreach( (l: Line) => logger.debug("expanded parsed macro: |" + l + "|"))
                macroInvocationLine :: parsedExpansionsWithInvocationLabel
        }

        def label: Parser[Label] = (
          ident <~ ":"
        ) ^^ {
            label =>
                if (debugParser) logger.debug("in label, ident: " + label)
                new Label(label)
        }

        def statement: Parser[Statement] = constantAssignment | variableAssignment | macroStart | origin | data |
            title | page | processor | align | condif1 | condelse | condendif | include | end | opcode | ignored

        // Not sure why I can't use ~> and <~ here to avoid the equ?
        def constantAssignment: Parser[ConstantAssignment] = (
            ident ~ equ ~ expression
            ) ^^ {
            case ident ~ _ ~ expression =>
                if (debugParser) logger.debug("in constantAssignment, ident: " + ident + " expr:" + expression)
                ConstantAssignment(ident.asInstanceOf[String], expression)
        }

        def variableAssignment: Parser[VariableAssignment] = (
          ident ~ "=" ~ expression
          ) ^^ {
            case ident ~ "=" ~ expression =>
                if (debugParser) logger.debug("in variableAssignment, ident: " + ident + " expr:" + expression)
                VariableAssignment(ident.asInstanceOf[String], expression)
        }

        def opcode: Parser[Statement] = {
            // if (debugParser) logger.debug("in opcode, processorToParse: " + processorToParse)

            processorToParse match {
                case None => noInstructionsAllowed
                case Some(Processor("TRANSPUTER")) => transputerInstruction
                case Some(Processor(_)) => noInstructionsAllowed
            }
        }

        def macroStart: Parser[MacroStart] = (
          ident ~ macroWord ~ repsep(ident, ",")
          ) ^^ {
            case ident ~ _ ~ parameters =>
                if (debugParser) logger.debug("in macroStart, ident: " + ident + " parameters:" + parameters)
                try {
                    val macroName = new MacroName(ident)
                    val macroParameterNames = parameters.map(new MacroParameterName(_))
                    macroManager.startMacro(macroName, macroParameterNames)
                    MacroStart(macroName, macroParameterNames)
                } catch {
                    case i: IllegalStateException => throw new AssemblyParserException(location, i.getMessage)
                }
        }

        def macroInvocation: Parser[MacroInvocation] = (
          existingMacro ~ repsep(macroArgument, macroArgumentSeparator)
        ) ^^ {
            case macroName ~ arguments =>
                if (debugParser) logger.debug("in macroInvocation, macro name: " + macroName + " arguments:" + arguments)
                MacroInvocation(macroName, arguments)
        }

        def macroArgumentSeparator: Parser[io.Serializable] = "," | rep(whiteSpace)

        // Special case to prevent re-detection of an existing macro (if the word following the existing macro name is MACRO)
        def existingMacro: Parser[MacroName] = ident ~ opt(macroWord) ^? {
            case possibleMacro ~ optMacroWord
                if macroManager.exists(possibleMacro) && optMacroWord.isEmpty =>
                if (debugParser) logger.debug("in existingMacro, ident: " + possibleMacro)
                possibleMacro
        }

        def macroArgument: Parser[MacroArgument] = (macroStringArgument | """[^\s,;]+""".r) ^^ {
            argument =>
                if (debugParser) logger.debug("in macroArgument, argument is [" + argument + "]")
                new MacroArgument(argument)
        }

        // The difference between these two and singleQuotedString is that these include the terminating quotes in the
        // parsed output. Macros must preserve this into their expansion.
        def singleQuotedMacroStringArgument: Parser[String] =
            """'([^'\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*'""".r ^^ {
                contents => {
                    if (debugParser) logger.debug("in singleQuotedMacroStringArgument, contents is: |" + contents + "|")
                    contents
                }
            }
        def doubleQuotedMacroStringArgument: Parser[String] =
            """"([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r ^^ {
                contents => {
                    if (debugParser) logger.debug("in doubleQuotedMacroStringArgument, contents is: |" + contents + "|")
                    contents
                }
            }

        def macroStringArgument: Parser[String] = singleQuotedMacroStringArgument | doubleQuotedMacroStringArgument

        def origin: Parser[Org] = (
          org ~> expression
        ) ^^ {
            expr =>
                if (debugParser) logger.debug("in org, addr:" + expr)
                Org(expr)
        }

        def data: Parser[Statement] = dbDup | dwDup | ddDup | db | dw | dd

        def db: Parser[DB] = (
          """(?i)DB""".r  ~> repsep(expression | characterExpression, ",")
          ) ^^ {
            exprs =>
                if (debugParser) logger.debug("in db, exprs:" + exprs)
                if (exprs.isEmpty) {
                    throw new AssemblyParserException(location, "DB directive without data")
                }
                DB(exprs)
        }

        def dup: Parser[Expression] = (
          """(?i)DUP\s*\(""".r ~> (expression | characterExpression) <~ """\)""".r
        ) ^^ {
            repeatedExpr =>
                if (debugParser) logger.debug("in dup, repeatedExpr:" + repeatedExpr)
                repeatedExpr
        }

        def dbDup: Parser[DBDup] = (
          """(?i)DB""".r ~> expression ~ dup
          ) ^^ {
            case countExpr ~ repeatedExpr =>
                if (debugParser) logger.debug("in dbDup, countExpr:" + countExpr + " repeatedExpr:" + repeatedExpr)
                DBDup(countExpr, repeatedExpr)
        }

        def dw: Parser[DW] = (
          """(?i)DW""".r  ~> repsep(expression | characterExpression, ",")
          ) ^^ {
            exprs =>
                if (debugParser) logger.debug("in dw, exprs:" + exprs)
                if (exprs.isEmpty) {
                    throw new AssemblyParserException(location, "DW directive without data")
                }
                DW(exprs)
        }

        def dwDup: Parser[DWDup] = (
          """(?i)DW""".r ~> expression ~ dup
          ) ^^ {
            case countExpr ~ repeatedExpr =>
                if (debugParser) logger.debug("in dwDup, countExpr:" + countExpr + " repeatedExpr:" + repeatedExpr)
                DWDup(countExpr, repeatedExpr)
        }

        def dd: Parser[DD] = (
          """(?i)DD""".r  ~> repsep(expression | characterExpression, ",")
          ) ^^ {
            exprs =>
                if (debugParser) logger.debug("in dd, exprs:" + exprs)
                if (exprs.isEmpty) {
                    throw new AssemblyParserException(location, "DD directive without data")
                }
                DD(exprs)
        }

        def ddDup: Parser[DDDup] = (
          """(?i)DD""".r ~> expression ~ dup
          ) ^^ {
            case countExpr ~ repeatedExpr =>
                if (debugParser) logger.debug("in ddDup, countExpr:" + countExpr + " repeatedExpr:" + repeatedExpr)
                DDDup(countExpr, repeatedExpr)
        }

        def title: Parser[Title] = (
          """(?i)TITLE""".r  ~> """.*""".r
        ) ^^ {
            text =>
                if (debugParser) logger.debug("in title, text:" + text)
                Title(text)
        }

        def page: Parser[Page] = (
          """(?i)PAGE""".r  ~> wholeNumber ~ "," ~ wholeNumber
          ) ^^ {
            case rows ~ _ ~ columns =>
                if (debugParser) logger.debug("in page, rows:" + rows + ", columns:" + columns)
                Page(rows.toInt, columns.toInt)
        }

        def processor: Parser[Processor] = """(?i)\.(386|TRANSPUTER)""".r ^^ {
            cpuString =>
                val cpu = cpuString.substring(1)
                if (debugParser) {
                    logger.debug("in processor, cpu string is '" + cpu + "'")
                }
                val out = Processor(cpu)
                cpu match {
                    case "386" =>
                        logger.warn("386 Processor; only its little-endianness is understood, none of its instructions")
                    case "TRANSPUTER" =>
                        processorToParse = Some(out) // Enables extra instruction set parser combinator.
                        logger.debug("Enabling Transputer opcodes, little-endianness")
                }
                out
        }

        def align: Parser[Align] = (
          """(?i)ALIGN""".r  ~> wholeNumber
          ) ^^ {
            alignment =>
                if (debugParser) logger.debug("in align, alignment:" + alignment)
                Align(alignment.toInt)
        }

        def end: Parser[End] = (
          """(?i)END""".r  ~> opt(expression)
          ) ^^ {
            expr =>
                if (debugParser) logger.debug("in end, expression:" + expr)
                End(expr)
        }

        def condif1: Parser[If1] = """(?i)IF1""".r ^^ ( _ => If1() )
        def condelse: Parser[Else] = """(?i)ELSE""".r ^^ ( _ => Else() )
        def condendif: Parser[Endif] = """(?i)ENDIF""".r ^^ ( _ => Endif() )

        def include: Parser[Include] = (
          """(?i)INCLUDE""".r  ~> includeFile
          ) ^^ {
            case fileName: Characters =>
                val includeFileName = fileName.text
                if (debugParser) logger.debug("including '" + includeFileName + "'")
                includer.pushIncludeFile(new File(includeFileName))
                Include(includeFileName)
        }

        def includeFile: Parser[Characters] = singleQuotedString | doubleQuotedString | nonWhiteSpaceSequence

        def ignored: Parser[Ignored] = ( ignoredKeyword ~ """.*""".r ) ^^ { _ => Ignored() }
        def ignoredKeyword: Regex = """(?i)(MAIN|ASSUME|\.LIST|\.NOLIST)""".r

        def characterExpression: Parser[Expression] = singleQuotedString | doubleQuotedString
        def singleQuotedString: Parser[Characters] =
            """'([^'\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*'""".r  ^^ {
                contents => {
                    val stringBody = contents.substring(1, contents.length - 1)
                    if (debugParser) logger.debug("in singleQuotedString, contents is: |" + stringBody + "|")
                    Characters(stringBody)
                }
            }
        def doubleQuotedString: Parser[Characters] =
            """"([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r ^^ {
                contents => {
                    val stringBody = contents.substring(1, contents.length - 1)
                    if (debugParser) logger.debug("in doubleQuotedString, contents is: |" + stringBody + "|")
                    Characters(stringBody)
                }
            }

        def nonWhiteSpaceSequence: Parser[Characters] =
            """\S*""".r ^^ {
                contents => {
                    if (debugParser) logger.debug("in nonWhiteSpaceSequence, contents is: |" + contents + "|")
                    Characters(contents)
                }
            }

        def equ: Parser[String] =
            """(?i)EQU""".r ^^ ( _ => "EQU" )

        def org: Parser[String] =
            """(?i)ORG""".r ^^ ( _ => "ORG" )

        def macroWord: Parser[String] =
            """(?i)MACRO""".r ^^ ( _ => "MACRO" )

        def comment: Parser[Comment] = doubleComment | singleComment

        def doubleComment: Parser[DoubleComment] =
            """;;.*""".r ^^ ( x => DoubleComment(x))

        def singleComment: Parser[SingleComment] =
            """;.*""".r ^^ ( x => SingleComment(x))
    }
}