/*
 * Copyright (C) 2008-2018 Matt Gumbley, DevZendo.org http://devzendo.org
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

import org.devzendo.tma.ast.AST.{Label, MacroArgument, MacroName, MacroParameterName}
import org.devzendo.tma.ast._
import org.log4s.Logger

import scala.util.matching.Regex
import scala.util.parsing.combinator._

class AssemblyParser(val debugParser: Boolean, val macroManager: MacroManager) {

    val logger: Logger = org.log4s.getLogger

    @throws(classOf[AssemblyParserException])
    def parse(line: String, lineNumber: Int, inMacroExpansion: Boolean = false): List[Line] = {

        def sanitizedInput = nullToEmpty(line).trim()

        if (debugParser) {
            logger.debug("parsing " + lineNumber + "|" + sanitizedInput + "|")
        }
        if (lineNumber < 1) {
            throw new AssemblyParserException(lineNumber, "Line numbers must be positive")
        }
        if (sanitizedInput.length > 0) {
            val parser = if (macroManager.isInMacroBody) new MacroBodyCombinatorParser(lineNumber) else new StatementCombinatorParser(lineNumber, inMacroExpansion)
            val parserOutput = parser.parseProgram(sanitizedInput)
            parserOutput match {
                case parser.Success(r, _) =>
                    val rLines = r.asInstanceOf[List[Line]] // unsure why r is not right type
                    if (debugParser) {
                        logger.debug("returning " + rLines)
                    }
                    rLines

                case x =>
                    logger.error(s"$lineNumber: ${x.toString}") // mostly a useless, hard to understand error...
                    throw new AssemblyParserException(lineNumber, "Unknown statement '" + sanitizedInput + "'")
            }

        } else {
            val line = Line(lineNumber, sanitizedInput, None, None)
            List(line)
        }
    }

    private def nullToEmpty(input: String): String = {
        if (input == null) "" else input

    }

    private trait LineParser extends JavaTokenParsers {
        var text: String = ""

        def line: Parser[List[Line]]

        def parseProgram(input: String): ParseResult[List[Line]] = {
            this.text = input
            parseAll(line, input)
        }
    }

    private class MacroBodyCombinatorParser(lineNumber: Int) extends LineParser {
        def line: Parser[List[Line]] = macroEnd | macroStart | macroBody

        def macroEnd: Parser[List[Line]] =
            """(?i)ENDM""".r ^^ {
                _ =>
                    if (debugParser) logger.debug("in endm")
                    macroManager.endMacro()
                    List(Line(lineNumber, text, None, Some(MacroEnd())))
            }

        def macroStart: Parser[List[Line]] = (
            ident ~ macroWord ~ repsep(ident, ",")
            ) ^^ {
                _ => throw new AssemblyParserException(lineNumber, "Macro definitions cannot be nested")
        }

        def macroWord: Parser[String] =
            """(?i)MACRO""".r ^^ ( _ => "MACRO" )

        def macroBody: Parser[List[Line]] =
        """.*""".r ^^ {
            x: String =>
                if (debugParser) logger.debug("in macroBody")
                macroManager.addMacroLine(x)
                List(Line(lineNumber, text, None, Some(MacroBody(x))))
        }
    }

    private class StatementCombinatorParser(lineNumber: Int, inMacroExpansion: Boolean) extends LineParser {
        def line: Parser[List[Line]] =  macroInvocationLine | statementLine

        def statementLine: Parser[List[Line]] = (
            opt(label) ~ opt(statement) ~ opt(comment)
          ) ^^ {
            case optLabel ~ optStatement ~ optComment =>
                if (debugParser) logger.debug("in statementLine, inMacroExpansion=" + inMacroExpansion + ", optComment=" + optComment)
                val returnedText = removeDoubleSemicolonCommentsInMacroExpansion(optComment)
                if (debugParser) logger.debug("in statementLine after ;;-removal, text=|" + returnedText + "|")
                List(Line(lineNumber, returnedText, optLabel, optStatement))
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

                val macroInvocationLine = Line(lineNumber, text, optLabel, Some(macroInvocation))

                val expansion = macroManager.expandMacro(macroInvocation.name, macroInvocation.args)
                if (debugParser) expansion.foreach( (f: String) => logger.debug("expanded macro: |" + f + "|"))
                val parsedExpansions = expansion.flatMap((str: String) => AssemblyParser.this.parse(str, lineNumber, inMacroExpansion = true))
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
            title | page | processor | align | condif1 | condelse | condendif | end | ignored

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
                    case i: IllegalStateException => throw new AssemblyParserException(lineNumber, i.getMessage)
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
            """'([^'\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*'""".r  ^^ {
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
                    throw new AssemblyParserException(lineNumber, "DB directive without data")
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
                    throw new AssemblyParserException(lineNumber, "DW directive without data")
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
                    throw new AssemblyParserException(lineNumber, "DD directive without data")
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

        def processor: Parser[Processor] = """(?i)\.(386|T800)""".r ^^ {
            cpuString =>
                val cpu = cpuString.substring(1)
                if (debugParser) {
                    logger.debug("in processor, cpu string is '" + cpu + "'")
                }
                Processor(cpu)
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

        def ignored: Parser[Ignored] = ( ignoredKeyword ~ """.*""".r ) ^^ { _ => Ignored() }
        def ignoredKeyword: Regex = """(?i)(MAIN|ASSUME|\.LIST|\.NOLIST)""".r

        def expression: Parser[Expression] = (
          term ~ rep("+" ~ term | "-" ~ term)
          ) ^^ {
            case term ~ rep => formBinary(term, rep)
        }

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

        def characterExpression: Parser[Expression] = singleQuotedString | doubleQuotedString

        def formBinary(factor: Expression, rep: List[~[String, Expression]]): Expression = {
            if (rep.isEmpty) {
                factor
            } else {
                if (debugParser) logger.debug("in formBinary, op is: " + rep.head._1.toUpperCase())

                val headOp = rep.head._1.toUpperCase match {
                    case "*" => Mult()
                    case "/" => Div()
                    case "+" => Add()
                    case "-" => Sub()
                    case "SHR" => ShiftRight()
                    case "SHL" => ShiftLeft()
                    case "AND" => And()
                    case "OR" => Or()
                }
                val headExpr = rep.head._2
                Binary(headOp, factor, formBinary(headExpr, rep.tail))
            }
        }

        def term: Parser[Expression] = (
          factor ~ rep("*" ~ factor | "/" ~ factor | shr ~ factor | shl ~ factor | and ~ factor | or ~ factor)
        ) ^^ {
            case factor ~ rep => formBinary(factor, rep)
        }

        def factor: Parser[Expression] = (
          opt("[!~-]".r) ~ factorBase
          )  ^^ {
            case optUnary ~ term =>
                optUnary match {
                    // A negative number can just be that, no need to unary negate it..
                    case Some("-") =>
                        term match {
                            case Number(n) =>
                                Number(-1 * n)
                            case _ =>
                                Unary(Negate(), term)
                        }
                    case Some("~") | Some("!") => Unary(Not(), term)
                    // regex ensures this can't happen
                    case Some(x) => throw new AssemblyParserException(lineNumber, "Unexpected 'unary' operator: '" + x + "'")
                    case None => term
                }
        }

        def factorBase: Parser[Expression] = (
          integer ^^ ( n => Number(n))
          | ident ^^ ( c => SymbolArg(c))
          | "(" ~> expression <~ ")"
        )

        def integer: Parser[Int] = hexIntegerOx | hexIntegerH | decimalInteger // order matters: 07F1FH could be 07 decimal

        // The parseLong conversion here ensures I can represent the maximum unsigned 32-bit values.
        def decimalInteger: Parser[Int] = """-?\d+(?!\.)""".r ^^ ( x => {
            if (debugParser) logger.debug("in decimalInteger(" + x + ")")
            (java.lang.Long.parseLong(x) & 0xffffffff).asInstanceOf[Int]
        })

        def hexIntegerOx: Parser[Int] = """0[xX]-?\p{XDigit}+\b""".r ^^ ( x => {
            if (debugParser) logger.debug("in hexIntegerOx(" + x + ")")
            (java.lang.Long.parseLong(x.substring(2), 16) & 0xffffffff).asInstanceOf[Int]
        })

        def hexIntegerH: Parser[Int] = """-?\p{XDigit}+[hH]\b""".r ^^ ( x => {
            if (debugParser) logger.debug("in hexIntegerH(" + x + ")")
            (java.lang.Long.parseLong(x.substring(0, x.length - 1), 16) & 0xffffffff).asInstanceOf[Int]
        })

        def equ: Parser[String] =
            """(?i)EQU""".r ^^ ( _ => "EQU" )

        def shr: Parser[String] =
            """(?i)(>>|SHR)""".r ^^ ( _ => "SHR" )

        def shl: Parser[String] =
            """(?i)(<<|SHL)""".r ^^ ( _ => "SHL" )

        def and: Parser[String] =
            """(?i)(&&|AND)""".r ^^ ( _ => "AND" )

        def or: Parser[String] =
            """(?i)(\|\||OR)""".r ^^ ( _ => "OR" )

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