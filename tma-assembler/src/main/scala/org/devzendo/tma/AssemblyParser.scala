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

import org.devzendo.tma.ast.AST.{MacroArgName, MacroName}
import org.devzendo.tma.ast._
import org.log4s.Logger

import scala.collection.mutable
import scala.util.parsing.combinator._

class AssemblyParser(val debugParser: Boolean, val macroManager: MacroManager) {

    val logger: Logger = org.log4s.getLogger

    private val lines = mutable.ArrayBuffer[Line]()
    def getCollectedLines: List[Line] = {
        lines.toList
    }


    @throws(classOf[AssemblyParserException])
    def parse(lineAndNumber: (String, Int)): Line = {
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
                    // TODO analyse
                    val rLine = r.asInstanceOf[Line] // unsure why r is not right type
                    if (debugParser) {
                        logger.debug("returning" + rLine)
                    }
                    lines += rLine
                    rLine
                case x => throw new AssemblyParserException(lineNumber, x.toString)
            }

        } else {
            val line = Line(lineNumber, sanitizedInput, None, None)
            lines += line
            line
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

        def line: Parser[Line]

        def parseProgram(input: String): ParseResult[Line] = {
            this.text = input
            parseAll(line, input)
        }
    }

    private class MacroBodyCombinatorParser(lineNumber: Int) extends LineParser {
        def line: Parser[Line] = macroEnd | macroStart | macroBody

        def macroEnd: Parser[Line] =
            """(endm|ENDM)""".r ^^ {
                _ =>
                    if (debugParser) logger.debug("in endm")
                    macroManager.endMacro()
                    Line(lineNumber, text, None, Some(MacroEnd()))
            }

        def macroStart: Parser[Line] = (
            ident ~ macroWord ~ repsep(ident, ",")
            ) ^^ {
                _ => throw new AssemblyParserException(lineNumber, "Macro definitions cannot be nested")
        }

        def macroWord: Parser[String] =
            """(macro|MACRO)""".r ^^ ( _ => "MACRO" )

        def macroBody: Parser[Line] =
        """.*""".r ^^ {
            x: String =>
                if (debugParser) logger.debug("in macroBody")
                macroManager.addMacroLine(x)
                Line(lineNumber, text, None, Some(MacroBody(x)))
        }
    }

    private class StatementCombinatorParser(lineNumber: Int) extends LineParser {
        def line: Parser[Line] = opt(statement) <~ opt(comment) ^^ {
            optStatement =>
                if (debugParser) logger.debug("in line")
                Line(lineNumber, text, None, optStatement)
        }

        def statement: Parser[Statement] = constantAssignment | variableAssignment | macroStart

        // Not sure why I can't use ~> and <~ here to avoid the equ?
        def constantAssignment: Parser[ConstantAssignment] = (
            ident ~ equ ~ expression
            ) ^^ {
            case ident ~ _ ~ expression =>
                if (debugParser) logger.debug("in constantAssignment, ident: " + ident + " expr:" + expression)
                ConstantAssignment(ident.asInstanceOf[String], expression)
                // TODO prevent reassignment to the same constant
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
            case ident ~ _ ~ args =>
                if (debugParser) logger.debug("in macroStart, ident: " + ident + " args:" + args)
                try {
                    val macroName = new MacroName(ident)
                    val macroArgs = args.map(new MacroArgName(_))
                    macroManager.startMacro(macroName, macroArgs)
                    MacroStart(macroName, macroArgs)
                } catch {
                    case i: IllegalStateException => throw new AssemblyParserException(lineNumber, i.getMessage)
                }
        }

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
          | ident ^^ ( c => SymbolArg(c)) // TODO or is it a variable, or label? or macro?
          | "(" ~> expression <~ ")"
        )

        def integer: Parser[Int] = hexIntegerOx | hexIntegerH | decimalInteger // order matters: 07F1FH could be 07 decimal

        def decimalInteger: Parser[Int] = """-?\d+(?!\.)""".r ^^ ( x => {
            if (debugParser) logger.debug("in decimalInteger(" + x + ")")
            Integer.parseInt(x)
        })

        def hexIntegerOx: Parser[Int] = """0[xX]-?\p{XDigit}+(?!\.)""".r ^^ ( x => {
            if (debugParser) logger.debug("in hexIntegerOx(" + x + ")")
            Integer.parseInt(x.substring(2), 16)
        })

        def hexIntegerH: Parser[Int] = """-?\p{XDigit}+[hH](?!\.)""".r ^^ ( x => {
            if (debugParser) logger.debug("in hexIntegerH(" + x + ")")
            Integer.parseInt(x.substring(0, x.length - 1), 16)
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

        def macroWord: Parser[String] =
            """(macro|MACRO)""".r ^^ ( _ => "MACRO" )

        def comment: Parser[String] =
            """;.*""".r
    }
}