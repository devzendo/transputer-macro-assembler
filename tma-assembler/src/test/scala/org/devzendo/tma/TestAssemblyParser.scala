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
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

class TestAssemblyParser extends AssertionsForJUnit with MustMatchers with MockitoSugar {
    val logger = org.log4s.getLogger

    val parser = new AssemblyParser(true)
    var lineNumber = 1

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    private def parseLine(line: String) = {
        parser.parse((line, lineNumber))
        lineNumber = lineNumber + 1
    }

    private def parseSingleLine(line: String): Line = {
        parseLine(line)
        val lines = parser.getLines()
        lines must have size 1
        lines.head
    }

    private def singleLineParsesToStatement(line: String, expectedStatement: Statement) = {
        parseSingleLine(line) must
          equal(Line(1, line.trim(), List.empty, None,
              Some(expectedStatement), None))
    }

    @Test
    def initial(): Unit = {
        parser.getLines() must be(empty)
    }

    @Test
    def positiveLineNumber(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("Line numbers must be positive")
        parser.parse((null, 0))
    }

    @Test
    def nullLine(): Unit = {
        parseSingleLine(null) must equal(Line(1, "", List.empty, None, None, None))
    }

    @Test
    def emptyLine(): Unit = {
        parseSingleLine("") must equal(Line(1, "", List.empty, None, None, None))
    }

    @Test
    def justAComment(): Unit = {
        parseSingleLine("  ; comment  ") must equal(Line(1, "; comment", List.empty, None, None, None))
    }

    @Test
    def incrementingLineNumbers(): Unit = {
        parseLine("  ; comment  ")
        parseLine("\t\t;;;another comment  ")
        val lines = parser.getLines()
        lines must have size 2
        lines.head must equal(Line(1, "; comment", List.empty, None, None, None))
        lines.tail.head must equal(Line(2, ";;;another comment", List.empty, None, None, None))
    }

    @Test
    def equHexConstantEndingInH(): Unit = {
        singleLineParsesToStatement("MASKK\t\tEQU\t07F1FH\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(0x07f1f)))
    }

    @Test
    def equHexConstantStartingIn0x(): Unit = {
        singleLineParsesToStatement("MASKK\t\tEQU\t0x07F1F\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(0x07f1f)))
    }

    @Test
    def equHexConstantStartingIn0X(): Unit = {
        singleLineParsesToStatement("MASKK\t\tEQU\t0X07F1F\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(0x07f1f)))
    }

    @Test
    def equHexConstantEndingInh(): Unit = {
        singleLineParsesToStatement("MASKK\t\tEQU\t07F1Fh\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(0x07f1f)))
    }

    @Test
    def equWithLowerCase(): Unit = {
        singleLineParsesToStatement("MASKK\t\tequ\t07F1FH\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(0x07f1f)))
    }

    @Test
    def equDecimalConstant(): Unit = {
        singleLineParsesToStatement("MASKK\t\tequ\t32543\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(32543)))
    }

    @Test
    def equNegativeDecimalConstant(): Unit = {
        singleLineParsesToStatement("MASKK\t\tequ\t-20\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(-20)))
    }

    @Test
    def equNegativeHexConstant0x(): Unit = {
        singleLineParsesToStatement("MASKK\t\tequ\t0x-20\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(-32)))
    }

    @Test
    def equNegativeHexConstantH(): Unit = {
        singleLineParsesToStatement("MASKK\t\tequ\t-20H\t\t\t;lexicon bit mask",
            ConstantAssignment("MASKK", Number(-32)))
    }

    @Test
    def equExpression(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM-256*CELLL\t\t;start of user area (UP0)",
            ConstantAssignment("UPP",
                Binary(Sub(), SymbolArg("EM"),
                    Binary(Mult(), Number(256), SymbolArg("CELLL")))))
    }

    @Test
    def variableInitialisedToNumber(): Unit = {
        singleLineParsesToStatement("_LINK\t= 0\t\t\t\t\t;force a null link",
            VariableAssignment("_LINK", Number(0)))
    }

    @Test
    def variableInitialisedToDollarSymbolic(): Unit = {
        singleLineParsesToStatement("\t_CODE\t= $\t\t\t\t;;save code pointer",
            VariableAssignment("_CODE", SymbolArg("$")))
    }

    @Test
    def variableInitialisedToExpression(): Unit = {
        val lenPlus3 = Binary(Add(), SymbolArg("_LEN"), Number(3))
        val lenPlus3TimesCelll = Binary(Mult(), lenPlus3, SymbolArg("CELLL"))
        val nameMinusAllThat = Binary(Sub(), SymbolArg("_NAME"), lenPlus3TimesCelll)
        singleLineParsesToStatement("\t_NAME\t= _NAME-((_LEN+3)*CELLL)\t;;new header on cell boundary",
            VariableAssignment("_NAME", nameMinusAllThat))
    }

    @Test
    def variableInitialisedToNegatedSymbolic(): Unit = {
        singleLineParsesToStatement("_CODE = -$",
            VariableAssignment("_CODE", Unary(Negate(), SymbolArg("$"))))
    }

    @Test
    def variableInitialisedToNotNumber(): Unit = {
        singleLineParsesToStatement("_MASK = ~10",
            VariableAssignment("_MASK", Unary(Not(), Number(10))))
    }

    @Test
    def variableInitialisedToNotNegatedNumber(): Unit = {
        singleLineParsesToStatement("_MASK = ~-10",
            VariableAssignment("_MASK", Unary(Not(), Number(-10))))
    }

    @Test
    def variableInitialisedToExpressionWithNotNegatedNumber(): Unit = {
        val div = Binary(Div(), Unary(Not(), Number(-10)), Number(17))
        val mult = Binary(Mult(), SymbolArg("_FROB"), div)
        val add = Binary(Add(), mult, SymbolArg("FOOBAR"))
        singleLineParsesToStatement("_MASK = _FROB * (~-10 / 17) + FOOBAR",
            VariableAssignment("_MASK", add))
    }
/*
Unary(~,Binary(/,Number(-10),Number(17))))
 did not equal
Binary(/,Unary(~,Number(-10)),Number(17)))

 */
    // TODO
    // shift left/right
    // logical ops
}
