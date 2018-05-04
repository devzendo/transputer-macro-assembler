/*
 * Copyright (C) 2008-2017 Matt Gumbley, DevZendo.org http://devzendo.org
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

package org.devzendo.tma

import org.devzendo.tma.ast.AST.{Label, MacroParameterName, MacroName, MacroArgument}
import org.devzendo.tma.ast._
import org.log4s.Logger

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.parsing.combinator._

class AssemblyParser(val debugParser: Boolean, val macroManager: MacroManager) {

    val logger: Logger = org.log4s.getLogger

    private val lines = mutable.ArrayBuffer[Line]()
    def getCollectedLines: List[Line] = {
        lines.toList
    }


    @throws(classOf[AssemblyParserException])
    def parse(lineAndNumber: (String, Int)): List[Line] = {
        val line = lineAndNumber._1
        val lineNumber = lineAndNumber._2

        def sanitizedInput = nullToEmpty(line).trim()

        if (debugParser) {
            logger.debug("parsing " + lineNumber + "|" + sanitizedInput + "|")
        }
        if (lineNumber < 1) {
            throw new AssemblyParserException(lineNumber, "Line numbers must be positive")
        }
        if (sanitizedInput.length > 0) {
            val parser = if (macroManager.isInMacroBody) new MacroBodyCombinatorParser(lineNumber) else new StatementCombinatorParser(lineNumber)
            val parserOutput = parser.parseProgram(sanitizedInput)
            parserOutput match {
                case parser.Success(r, _) =>
                    val rLines = r.asInstanceOf[List[Line]] // unsure why r is not right type
                    if (debugParser) {
                        logger.debug("returning" + rLines)
                    }
                    lines ++= rLines
                    rLines

                case x =>
                    logger.error(s"$lineNumber: ${x.toString}") // mostly a useless, hard to understand error...
                    throw new AssemblyParserException(lineNumber, "Unknown statement '" + sanitizedInput + "'")
            }

        } else {
            val line = Line(lineNumber, sanitizedInput, None, None)
            lines += line
            List(line)
        }
    }

    private def nullToEmpty(input: String): String = {
        if (input == null) "" else input

    }

    def createModel: AssemblyModel = {
        new AssemblyModel
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
            """(endm|ENDM)""".r ^^ {
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
            """(macro|MACRO)""".r ^^ ( _ => "MACRO" )

        def macroBody: Parser[List[Line]] =
        """.*""".r ^^ {
            x: String =>
                if (debugParser) logger.debug("in macroBody")
                macroManager.addMacroLine(x)
                List(Line(lineNumber, text, None, Some(MacroBody(x))))
        }
    }

    private class StatementCombinatorParser(lineNumber: Int) extends LineParser {
        def line: Parser[List[Line]] = (
            opt(label) ~ opt(statement) <~ opt(comment)
          ) ^^ {
            case optLabel ~ optStatement =>
                if (debugParser) logger.debug("in line")
                List(Line(lineNumber, text, optLabel, optStatement))
        }

        def label: Parser[Label] = (
          ident <~ ":"
        ) ^^ {
            label =>
                if (debugParser) logger.debug("in label, ident: " + label)
                new Label(label)
        }

        def statement: Parser[Statement] = constantAssignment | variableAssignment | macroStart | macroInvocation | origin | data |
            title | page | align | ignored

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
          existingMacro ~ repsep(macroArgument, """[\s,]""".r)
        ) ^^ {
            case macroName ~ parameters =>
                if (debugParser) logger.debug("in macroInvocation, macro name: " + macroName + " args:" + parameters)
                MacroInvocation(macroName, parameters)
        }

        def existingMacro: Parser[MacroName] = ident ^? {
            case possibleMacro
                if macroManager.exists(possibleMacro) =>
                if (debugParser) logger.debug("in existingMacro, ident: " + possibleMacro)
                possibleMacro
        }

        def macroArgument: Parser[MacroArgument] = """[^\s,]+""".r ^^ { // TODO what about ( expressions ) ?
            argument =>
                if (debugParser) logger.debug("in macroArgument, argument is [" + argument + "]")
                new MacroArgument(argument)
        }

        def origin: Parser[Org] = (
          org ~> expression
        ) ^^ {
            expr =>
                if (debugParser) logger.debug("in org, addr:" + expr)
                Org(expr)
        }

        def data: Parser[Statement] = db | dw | dd

        def db: Parser[DB] = (
          """(db|DB)""".r  ~> repsep(expression, ",")
          ) ^^ {
            exprs =>
                if (debugParser) logger.debug("in db, exprs:" + exprs)
                if (exprs.isEmpty) {
                    throw new AssemblyParserException(lineNumber, "DB directive without data")
                }
                DB(exprs)
        }

        def dw: Parser[DW] = (
          """(dw|DW)""".r  ~> repsep(expression, ",")
          ) ^^ {
            exprs =>
                if (debugParser) logger.debug("in dw, exprs:" + exprs)
                if (exprs.isEmpty) {
                    throw new AssemblyParserException(lineNumber, "DW directive without data")
                }
                DW(exprs)
        }

        def dd: Parser[DD] = (
          """(dd|DD)""".r  ~> repsep(expression, ",")
          ) ^^ {
            exprs =>
                if (debugParser) logger.debug("in dd, exprs:" + exprs)
                if (exprs.isEmpty) {
                    throw new AssemblyParserException(lineNumber, "DD directive without data")
                }
                DD(exprs)
        }

        def title: Parser[Title] = (
          """(title|TITLE)""".r  ~> """.*""".r
        ) ^^ {
            text =>
                if (debugParser) logger.debug("in title, text:" + text)
                Title(text)
        }

        def page: Parser[Page] = (
          """(page|PAGE)""".r  ~> wholeNumber ~ "," ~ wholeNumber
          ) ^^ {
            case rows ~ _ ~ columns =>
                if (debugParser) logger.debug("in page, rows:" + rows + ", columns:" + columns)
                Page(rows.toInt, columns.toInt)
        }

        def align: Parser[Align] = (
          """(align|ALIGN)""".r  ~> wholeNumber
          ) ^^ {
            alignment =>
                if (debugParser) logger.debug("in align, alignment:" + alignment)
                Align(alignment.toInt)
        }

        def ignored: Parser[Ignored] = ( ignoredKeyword ~ """.*""".r ) ^^ { _ => Ignored() }
        def ignoredKeyword: Regex = """(main|MAIN|assume|ASSUME)""".r

        def expression: Parser[Expression] = (
          term ~ rep("+" ~ term | "-" ~ term)
          ) ^^ {
            case term ~ rep => formBinary(term, rep)
        }

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

        def hexIntegerOx: Parser[Int] = """0[xX]-?\p{XDigit}+(?!\.)""".r ^^ ( x => {
            if (debugParser) logger.debug("in hexIntegerOx(" + x + ")")
            (java.lang.Long.parseLong(x.substring(2), 16) & 0xffffffff).asInstanceOf[Int]
        })

        def hexIntegerH: Parser[Int] = """-?\p{XDigit}+[hH](?!\.)""".r ^^ ( x => {
            if (debugParser) logger.debug("in hexIntegerH(" + x + ")")
            (java.lang.Long.parseLong(x.substring(0, x.length - 1), 16) & 0xffffffff).asInstanceOf[Int]
        })

        def equ: Parser[String] =
            """(equ|EQU)""".r ^^ ( _ => "EQU" )

        def shr: Parser[String] =
            """(>>|shr|SHR)""".r ^^ ( _ => "SHR" )

        def shl: Parser[String] =
            """(<<|shl|SHL)""".r ^^ ( _ => "SHL" )

        def and: Parser[String] =
            """(&&|and|AND)""".r ^^ ( _ => "AND" )

        def or: Parser[String] =
            """(\|\||or|OR)""".r ^^ ( _ => "OR" )

        def org: Parser[String] =
            """(org|ORG)""".r ^^ ( _ => "ORG" )

        def macroWord: Parser[String] =
            """(macro|MACRO)""".r ^^ ( _ => "MACRO" )

        def comment: Parser[String] =
            """;.*""".r
    }
}