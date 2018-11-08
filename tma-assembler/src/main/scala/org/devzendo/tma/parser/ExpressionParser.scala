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

import org.devzendo.tma.ast._
import org.log4s.Logger

import scala.util.parsing.combinator.JavaTokenParsers

trait ExpressionParser extends JavaTokenParsers with DiagnosableParser {

    val logger: Logger = org.log4s.getLogger

    /*
     * Precedence table (subset supported by this assembler):
     * Operations in parentheses are performed before adjacent operations
     * Binary operations of highest precedence are first
     * Operations of equal precedence are performed left to right
     * Unary operations of equal precedence are performed left to right
     * 1:  ()
     * 7:  +, - (unary)
     * 8:  *, /, SHR, SHL
     * 9:  +, - (binary)
     * 11: NOT
     * 12: AND
     * 13: OR
     */

    private def opStrToOperator(opStr: String) = opStr.toUpperCase match {
        case "*" => Mult()
        case "/" => Div()
        case "+" => Add()
        case "-" => Sub()
        case "SHR" => ShiftRight()
        case "SHL" => ShiftLeft()
        case "!" | "~" => Not()
        case "AND" => And()
        case "OR" => Or()
    }

    // With thanks to Christoph Henkelmann for insights into parsing expression precedence..
    // https://github.com/chenkelmann/parser_example/blob/master/src/main/scala/eu/henkelmann/parser02/ExpressionParsers.scala

    def primary: Parser[Expression] = (
      integer ^^ ( n => Number(n))
        | ident ^^ ( c => SymbolArg(c))
        | "(" ~> expression <~ ")"
      )

    /*def unaryE: Parser[Expression] = primary | (( "~" | "!" | "+" | "-" ) ~ unaryE) ^^ {
        case op ~ e => Unary(opStrToOperator(op), e)
    }*/

    def unaryE: Parser[Expression] = (
      opt("""(?i)(OFFSET|[!~-])""".r) ~ primary
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
                case Some(x) =>
                    x.toUpperCase match {
                        case "OFFSET" =>
                            Unary(Offset(), term)
                        case _ =>
                            throw new AssemblyParserException(0 /*lineNumber*/, "Unexpected 'unary' operator: '" + x + "'")
                    }
                case None => term
            }
    }

    def multiplicationE: Parser[Expression] = (unaryE ~ rep(( "*" | "/" | shr | shl ) ~ unaryE)) ^^ { foldExpressions }

    def additionE: Parser[Expression] = (multiplicationE ~ rep( ("+" | "-" ) ~ multiplicationE)) ^^ { foldExpressions }

    def andE: Parser[Expression] = (additionE ~ rep(and ~ additionE)) ^^ { foldExpressions }

    def orE: Parser[Expression] = (andE ~ rep(or ~ andE)) ^^ { foldExpressions }

    def expression: Parser[Expression] = orE

    def foldExpressions(result: ExpressionParser.this.~[Expression,List[ExpressionParser.this.~[String,Expression]]]): Expression =
        result match {
            case e ~ list => list.foldLeft(e) ((exp,el) => Binary(opStrToOperator(el._1), exp, el._2))
        }


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

    def shr: Parser[String] =
        """(?i)(>>|SHR)""".r ^^ ( _ => "SHR" )

    def shl: Parser[String] =
        """(?i)(<<|SHL)""".r ^^ ( _ => "SHL" )

    def and: Parser[String] =
        """(?i)(&&|AND)""".r ^^ ( _ => "AND" )

    def or: Parser[String] =
        """(?i)(\|\||OR)""".r ^^ ( _ => "OR" )


}
