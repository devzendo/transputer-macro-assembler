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

class AssemblyParser(val debugParser: Boolean) {

    val logger: Logger = org.log4s.getLogger

    var lineNumber: Int = 0

    private val lines = mutable.ArrayBuffer[Line]()
    def getLines: List[Line] = {
        lines.toList
    }

    // State for built macros
    private val macros = mutable.Map[MacroName, MacroDefinition]()
    def getMacro(macroName: MacroName): Option[MacroDefinition] = macros.get(macroName)

    // State for macro definition buildup
    private var inMacroBody = false
    def isInMacroBody: Boolean = inMacroBody
    private var macroName = new MacroName("")
    private val macroArgs = mutable.ArrayBuffer[MacroArgName]()
    def getMacroArgs: List[MacroArgName] = macroArgs.toList
    private val macroLines = mutable.ArrayBuffer[String]()
    def getMacroLines: List[String] = macroLines.toList


    @throws(classOf[AssemblyParserException])
    def parse(lineAndNumber: (String, Int)): Line = {
        val line = lineAndNumber._1
        val number = lineAndNumber._2

        def sanitizedInput = nullToEmpty(line).trim()

        if (debugParser) {
            logger.debug("parsing " + number + "|" + sanitizedInput + "|")
        }
        if (number < 1) {
            throw new AssemblyParserException("Line numbers must be positive")
        }
        if (sanitizedInput.length > 0) {
            val parser = if (inMacroBody) new MacroBodyCombinatorParser() else new StatementCombinatorParser()
            val parserOutput = parser.parseProgram(number, sanitizedInput)
            parserOutput match {
                case parser.Success(r, _) =>
                    // TODO analyse
                    val rLine = r.asInstanceOf[Line] // unsure why r is not right type
                    if (debugParser) {
                        logger.debug("returning" + rLine)
                    }
                    lines += rLine
                    rLine
                case x => throw new AssemblyParserException(x.toString)
            }

        } else {
            val line = Line(number, sanitizedInput, List.empty, None, None, None)
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

        def parseProgram(currentLineNumber: Int, input: String): ParseResult[Line] = {
            lineNumber = currentLineNumber
            this.text = input
            parseAll(line, input)
        }
    }

    private class MacroBodyCombinatorParser extends LineParser {
        def line: Parser[Line] = macroEnd | macroBody

        def macroEnd: Parser[Line] =
            """(endm|ENDM)""".r ^^ {
                _ =>
                    if (debugParser) logger.debug("in endm")
                    inMacroBody = false
                    val definition = MacroDefinition(macroName, macroArgs.toList, macroLines.toList)
                    macros(macroName) = definition
                    macroArgs.clear()
                    macroLines.clear()
                    Line(lineNumber, text, List.empty, None, Some(MacroEnd()), None)
            }

        def macroBody: Parser[Line] =
        """.*""".r ^^ {
            x: String =>
                if (debugParser) logger.debug("in macroBody")
                macroLines += x
                Line(lineNumber, text, List.empty, None, Some(MacroBody(x)), None)
        }
    }

    private class StatementCombinatorParser extends LineParser {
        def line: Parser[Line] = opt(statement) <~ opt(comment) ^^ {
            optStatement =>
                if (debugParser) logger.debug("in line")
                Line(lineNumber, text, List.empty, None, optStatement, None)
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
                inMacroBody = true
                macroLines.clear()
                macroArgs.++=:(args.map(new MacroArgName(_)))
                macroName = new MacroName(ident)
                MacroStart(macroName, args.map(new MacroArgName(_)))
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
                    case Some(x) => throw new AssemblyParserException("Unexpected 'unary' operator: '" + x + "'")
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