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

import org.devzendo.tma.ast._

import scala.collection.mutable
import scala.util.parsing.combinator._

class AssemblyParser(val debugParser: Boolean) {

    val logger = org.log4s.getLogger

    val lines = mutable.ArrayBuffer[Line]()

    def getLines() = {
        lines.toList
    }

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
            val ccp = new StatementCombinatorParser()
            val parserOutput = ccp.parseProgram(number, sanitizedInput)
            parserOutput match {
                case ccp.Success(r, _) => {
                    // TODO analyse
                    val rLine = r.asInstanceOf[Line] // unsure why r is not right type
                    if (debugParser) {
                        logger.debug("returning" + rLine)
                    }
                    lines += rLine
                    rLine
                }
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

    private class StatementCombinatorParser extends JavaTokenParsers {
        var lineNumber: Int = 0
        var text: String = ""

        def line: Parser[Line] = opt(statement) ~ opt(comment) ^^ {
            case optStatement ~ optComment =>
                if (debugParser) logger.debug("in line")
                Line(lineNumber, text, List.empty, None, optStatement, None)

        }

        def statement: Parser[Statement] = (
          constantAssignment | variableAssignment
        )

        // Not sure why I can't use ~> and <~ here to avoid the equ?
        def constantAssignment: Parser[ConstantAssignment] = (
            ident ~ equ ~ expression
            ) ^^ {
            case ident ~ equ ~ expression =>
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

        def expression: Parser[Expression] = (
          term ~ rep("+" ~ term | "-" ~ term)
        )  ^^ {
            case term ~ rep => formBinary(term, rep)
        }

        def formBinary(factor: Expression, rep: List[~[String, Expression]]): Expression = {
            if (rep.isEmpty) {
                factor
            } else {
                val headOp = rep.head._1 match {
                    case "*" => Mult()
                    case "/" => Div()
                    case "+" => Add()
                    case "-" => Sub()
                }
                val headExpr = rep.head._2
                Binary(headOp, factor, formBinary(headExpr, rep.tail))
            }
        }

        def term: Parser[Expression] = (
          factor ~ rep("*" ~ factor | "/" ~ factor)
        ) ^^ {
            case factor ~ rep => formBinary(factor, rep)
        }

        def factor: Parser[Expression] = (
          integer ^^ ( n => Number(n))
          | ident ^^ ( c => SymbolArg(c)) // TODO or is it a variable, or label? or macro?
          | "(" ~> expression <~ ")"
        )

        def integer: Parser[Int] = (hexIntegerOx | hexIntegerH | decimalInteger) // order matters: 07F1FH could be 07 decimal

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
            """(equ|EQU)""".r

        def comment: Parser[String] =
            """;.*""".r

        def parseProgram(lineNumber: Int, input: String) = {
            this.lineNumber = lineNumber
            this.text = input
            parseAll(line, input)
        }
    }
}