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
import org.devzendo.tma.ast.AST._
import org.devzendo.tma.ast._
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

class TestAssemblyParser extends AssertionsForJUnit with MustMatchers with MockitoSugar {
    val logger: Logger = org.log4s.getLogger

    val macroManager = new MacroManager(true)
    val parser = new AssemblyParser(true, macroManager)
    var lineNumber = 1

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    private def parseLine(line: String) = {
        val out = parser.parse(line, lineNumber)
        lineNumber = lineNumber + 1
        out
    }

    private def parseLines(lines: List[String]): List[Line] = {
        lines.foreach(parseLine)
        parser.getCollectedLines
    }

    private def parseSingleLine(line: String): Line = {
        parseLine(line)
        val lines = parser.getCollectedLines
        lines must have size 1
        lines.head
    }

    private def singleLineParsesToStatement(line: String, expectedStatement: Statement): Unit = {
        parseSingleLine(line) must
          equal(Line(1, line.trim(), None, Some(expectedStatement)))
    }

    @Test
    def initial(): Unit = {
        parser.getCollectedLines must be(empty)
    }

    @Test
    def positiveLineNumber(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("0: Line numbers must be positive")
        parser.parse(null, 0)
    }

    @Test
    def nullLine(): Unit = {
        parseSingleLine(null) must equal(Line(1, "", None, None))
    }

    @Test
    def emptyLine(): Unit = {
        parseSingleLine("") must equal(Line(1, "", None, None))
    }

    @Test
    def justAComment(): Unit = {
        parseSingleLine("  ; comment  ") must equal(Line(1, "; comment", None, None))
    }

    @Test
    def incrementingLineNumbers(): Unit = {
        parseLine("  ; comment  ")
        parseLine("\t\t;;;another comment  ")
        val lines = parser.getCollectedLines
        lines must have size 2
        lines.head must equal(Line(1, "; comment", None, None))
        lines.tail.head must equal(Line(2, ";;;another comment", None, None))
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

    @Test
    def shiftRight(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM SHR 5",
            ConstantAssignment("UPP",
                Binary(ShiftRight(), SymbolArg("EM"), Number(5))))
    }

    @Test
    def shiftRightWithLowerCase(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM shr 5",
            ConstantAssignment("UPP",
                Binary(ShiftRight(), SymbolArg("EM"), Number(5))))
    }

    @Test
    def shiftRightSymbol(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM >> 5",
            ConstantAssignment("UPP",
                Binary(ShiftRight(), SymbolArg("EM"), Number(5))))
    }

    @Test
    def shiftLeft(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM SHL 2",
            ConstantAssignment("UPP",
                Binary(ShiftLeft(), SymbolArg("EM"), Number(2))))
    }

    @Test
    def shiftLeftWithLowerCase(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM shl 2",
            ConstantAssignment("UPP",
                Binary(ShiftLeft(), SymbolArg("EM"), Number(2))))
    }

    @Test
    def shiftLeftSymbol(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM << 2",
            ConstantAssignment("UPP",
                Binary(ShiftLeft(), SymbolArg("EM"), Number(2))))
    }

    @Test
    def andWord(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM AND 7",
            ConstantAssignment("UPP",
                Binary(And(), SymbolArg("EM"), Number(7))))
    }

    @Test
    def andWordWithLowerCase(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM and 7",
            ConstantAssignment("UPP",
                Binary(And(), SymbolArg("EM"), Number(7))))
    }

    @Test
    def andSymbol(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM && 7",
            ConstantAssignment("UPP",
                Binary(And(), SymbolArg("EM"), Number(7))))
    }

    @Test
    def orWord(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM OR 7",
            ConstantAssignment("UPP",
                Binary(Or(), SymbolArg("EM"), Number(7))))
    }

    @Test
    def orWordWithLowerCase(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM or 7",
            ConstantAssignment("UPP",
                Binary(Or(), SymbolArg("EM"), Number(7))))
    }

    @Test
    def orSymbol(): Unit = {
        singleLineParsesToStatement("UPP\t\tEQU\tEM || 7",
            ConstantAssignment("UPP",
                Binary(Or(), SymbolArg("EM"), Number(7))))
    }

    @Test
    def orgDirectiveNumber(): Unit = {
        singleLineParsesToStatement("ORG\t0x4000000", Org(Number(0x4000000)))
    }

    @Test
    def orgDirectiveExpression(): Unit = {
        singleLineParsesToStatement("ORG\tXYZ + 40", Org(Binary(Add(), SymbolArg("XYZ"), Number(40))))
    }

    @Test
    def orgDirectiveSymbol(): Unit = {
        singleLineParsesToStatement("ORG\tXYZ", Org(SymbolArg("XYZ")))
    }

    @Test
    def dbZero(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("1: DB directive without data")
        parser.parse("DB\t", 1)
    }

    @Test
    def dbSingle(): Unit = {
        singleLineParsesToStatement("DB\t0x00", DB(List(Number(0))))
    }

    @Test
    def dbExpression(): Unit = {
        singleLineParsesToStatement("DB\tXYZ + 4", DB(List(Binary(Add(), SymbolArg(new SymbolName("XYZ")), Number(4)))))
    }

    @Test
    def dbMultiple(): Unit = {
        singleLineParsesToStatement("DB\t1,2,3,4", DB(List(Number(1), Number(2), Number(3), Number(4))))
    }

    @Test
    def dbDuplicate(): Unit = {
        singleLineParsesToStatement("DB\t4 DUP(3)", DBDup(Number(4), Number(3)))
    }

    @Test
    def dbSingleQuotedCharacters(): Unit = {
        singleLineParsesToStatement("DB\t'Unga Bunga'", DB(List(Characters("Unga Bunga"))))
    }

    @Test
    def dbSingleQuotedEmptyString(): Unit = {
        singleLineParsesToStatement("DB\t''", DB(List(Characters(""))))
    }

    @Test
    def dbDoubleQuotedCharacters(): Unit = {
        singleLineParsesToStatement("DB\t\"Unga Bunga\"", DB(List(Characters("Unga Bunga"))))
    }

    @Test
    def dbDoubleQuotedEmptyString(): Unit = {
        singleLineParsesToStatement("DB\t\"\"", DB(List(Characters(""))))
    }

    @Test
    def dbMixingExpressionAndStringDisallowed(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("1: Unknown statement 'DB\t'foo' + 3'") // Not the clearest message, but disallowing is clear.

        parser.parse("DB\t'foo' + 3", 1)
    }

    @Test
    def dbSingleOverflow(): Unit = {
        // This is allowed by the parser - it could be some expression we don't know the final value of, so
        // don't disallow it here, deal with it in code generation.
        singleLineParsesToStatement("DB\t0x100", DB(List(Number(256))))
    }

    @Test
    def dwZero(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("1: DW directive without data")
        parser.parse("DW\t", 1)
    }

    @Test
    def dwSingle(): Unit = {
        singleLineParsesToStatement("DW\t0x0000", DW(List(Number(0))))
    }

    @Test
    def dwMultiple(): Unit = {
        singleLineParsesToStatement("DW\t1,2,3,4", DW(List(Number(1), Number(2), Number(3), Number(4))))
    }

    @Test
    def dwDuplicate(): Unit = {
        singleLineParsesToStatement("DW\t4 DUP(3)", DWDup(Number(4), Number(3)))
    }

    @Test
    def dwSingleOverflow(): Unit = {
        // This is allowed by the parser - it could be some expression we don't know the final value of, so
        // don't disallow it here, deal with it in code generation.
        singleLineParsesToStatement("DW\t0x10000", DW(List(Number(0x10000))))
    }

    @Test
    def ddZero(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("1: DD directive without data")
        parser.parse("DD\t", 1)
    }

    @Test
    def ddSingle(): Unit = {
        singleLineParsesToStatement("DD\t0x0", DD(List(Number(0))))
    }

    @Test
    def ddSingleMaxUnsignedHex0x(): Unit = {
        singleLineParsesToStatement("DD\t0xffffffff", DD(List(Number(0xffffffff))))
    }

    @Test
    def ddSingleMaxUnsignedHexH(): Unit = {
        singleLineParsesToStatement("DD\tffffffffH", DD(List(Number(0xffffffff))))
    }

    @Test
    def ddSingleMaxUnsignedDecimal(): Unit = {
        singleLineParsesToStatement("DD\t4294967295", DD(List(Number(0xffffffff))))
    }

    @Test
    def ddMultiple(): Unit = {
        singleLineParsesToStatement("DD\t1,2,3,4", DD(List(Number(1), Number(2), Number(3), Number(4))))
    }

    @Test
    def ddDuplicate(): Unit = {
        singleLineParsesToStatement("DD\t4 DUP(3)", DDDup(Number(4), Number(3)))
    }

    // ddSingleOverflow cannot be expressed since we use Ints, and 0x100000000 doesn't fit in one.

    @Test
    def whatTheActual(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("1: Unknown statement 'blarf!'")
        parser.parse("blarf!", 1)
    }

    @Test
    def title(): Unit = {
        singleLineParsesToStatement("TITLE InMos Transputer 32-bit eForth", Title("InMos Transputer 32-bit eForth"))
    }

    @Test
    def page(): Unit = {
        singleLineParsesToStatement("PAGE 60,132\t;60 lines per page, 132 characters per line ( was 62,132 )", Page(60, 132))
    }

    @Test
    def align(): Unit = {
        singleLineParsesToStatement("ALIGN 4 ; cell boundary", Align(4))
    }

    @Test
    def end(): Unit = {
        singleLineParsesToStatement("END\tORIG", End(SymbolArg("ORIG")))
    }

    @Test
    def conditionalIf1(): Unit = {
        singleLineParsesToStatement("IF1", If1())
    }

    @Test
    def conditionalElse(): Unit = {
        singleLineParsesToStatement("ELSE", Else())
    }

    @Test
    def conditionalEndif(): Unit = {
        singleLineParsesToStatement("ENDIF", Endif())
    }

    // This is a simple macro assembler for (initially) a simple, non-segmented architecture. If you need to add
    // directives to enable better assembly for x86, I'm happy to accept pull requests and tests. If you want to turn
    // it into a full, open source precise MASM clone... let's talk :)
    @Test
    def main(): Unit = {
        singleLineParsesToStatement("MAIN\tSEGMENT USE32", Ignored())
    }

    @Test
    def assume(): Unit = {
        singleLineParsesToStatement("ASSUME\tCS:MAIN", Ignored())
    }

    @Test
    def label(): Unit = {
        val line = "FOO:\tDD\t0x0"
        parseSingleLine(line) must
          equal(Line(1, line.trim(), Some(new Label("FOO")), Some(DD(List(Number(0))))))
    }


    private val expectedMacroParameterNames = List(new MacroParameterName("LEX"), new MacroParameterName("NAME"), new MacroParameterName("LABEL"))

    @Test
    def macroStartWithParameterNames(): Unit = {
        singleLineParsesToStatement("$CODE\tMACRO\tLEX,NAME,LABEL",
            MacroStart(new MacroName("$CODE"), expectedMacroParameterNames))
    }

    @Test
    def macroStartWithNoParameterNames(): Unit = {
        singleLineParsesToStatement("$CODE\tMACRO\t",
            MacroStart(new MacroName("$CODE"), List.empty))
    }

    @Test
    def macroStartBodyAndEnd(): Unit = {
        val codeMacroName = "$CODE"

        val textLines = List(
            "$CODE\tMACRO\tLEX,NAME,LABEL",
            "\t_CODE\t= $\t\t\t\t;;save code pointer",
            "\t_LEN\t= (LEX AND 01FH)/CELLL\t\t;;string cell count, round down",
            "\t_NAME\t= _NAME-((_LEN+3)*CELLL)\t;;new header on cell boundary"
        )

        val lines = parseLines(textLines)
        lines must have size 4

        val expectedStartStatement = MacroStart(new MacroName("$CODE"), expectedMacroParameterNames)
        lines(0) must equal(Line(1, textLines(0).trim(), None, Some(expectedStartStatement)))

        lines(1) must equal(Line(2, textLines(1).trim(), None, Some(MacroBody(textLines(1).trim()))))

        lines(2) must equal(Line(3, textLines(2).trim(), None, Some(MacroBody(textLines(2).trim()))))

        lines(3) must equal(Line(4, textLines(3).trim(), None, Some(MacroBody(textLines(3).trim()))))

        val expectedMacroLines = List(
            textLines(1).trim(), textLines(2).trim(), textLines(3).trim()
        )
        macroManager.getMacro(codeMacroName) must be(None) // not Some until ENDM

        // Now end the macro....
        val endmText = "\tENDM"
        val endmLines = parseLine(endmText)
        endmLines must equal(List(Line(5, endmText.trim(), None, Some(MacroEnd()))))

        macroManager.getMacro(codeMacroName) must be(Some(MacroDefinition(new MacroName(codeMacroName), expectedMacroParameterNames, expectedMacroLines)))
    }

    @Test
    def nestedMacroDefinitions(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("2: Macro definitions cannot be nested")
        parser.parse("$CODE\tMACRO\tLEX,NAME,LABEL", 1)
        parser.parse("$COLON\tMACRO\tLEX,NAME,LABEL", 2)
    }

    @Test
    def macroCannotBeRedefined(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("3: Macro '$CODE' already defined")
        parser.parse("$CODE\tMACRO\tLEX,NAME,LABEL", 1)
        parser.parse("\tendm", 2)
        parser.parse("$CODE\tMACRO\tLEX,NAME,LABEL", 3)
    }

    @Test
    def macroInvocationWithArgumentsSeparatedByCommas(): Unit = {
        val textLines = List(
            "$CODE\tMACRO\tLEX,NAME,LABEL",
            "\tENDM",
            "\t\t$CODE\t3,'URD',urdcode"
        )

        checkCorrectMacroInvocation(textLines)
    }

    @Test
    def macroInvocationWithArgumentsSeparatedBySpaces(): Unit = {
        val textLines = List(
            "$CODE\tMACRO\tLEX,NAME,LABEL",
            "\tENDM",
            "\t\t$CODE\t3\t  \t\t \t\t'URD'\turdcode"
        )

        checkCorrectMacroInvocation(textLines)
    }

    @Test
    def macroInvocationWithArgumentsFollowedBySingleComment(): Unit = {
        val textLines = List(
            "$CODE\tMACRO\tLEX,NAME,LABEL",
            "\tENDM",
            "\t\t$CODE\t3\t  \t\t \t\t'URD'\turdcode\t; comment"
        )

        val lines = checkCorrectMacroInvocation(textLines)
        val invocationLine = lines.last
        invocationLine.stmt match {
            case Some(MacroInvocation(_, args)) =>
                args.size must be(3) // not 4 or 5, with the fourth being the comment delimiter ; and fifth 'comment'
            case _ =>
                fail("Was expecting a MacroInvocation")
        }
        invocationLine.text must be("$CODE\t3\t  \t\t \t\t'URD'\turdcode\t; comment")
    }

    @Test
    def macroInvocationWithArgumentsFollowedByDoubleComment(): Unit = {
        val textLines = List(
            "$CODE\tMACRO\tLEX,NAME,LABEL",
            "\tENDM",
            "\t\t$CODE\t3\t  \t\t \t\t'URD'\turdcode\t;; comment"
        )

        val lines = checkCorrectMacroInvocation(textLines)
        val invocationLine = lines.last
        invocationLine.stmt match {
            case Some(MacroInvocation(_, args)) =>
                args.size must be(3) // not 4 or 5, with the fourth being the comment delimiter ;; and fifth 'comment'
            case _ =>
                fail("Was expecting a MacroInvocation")
        }
        invocationLine.text must be("$CODE\t3\t  \t\t \t\t'URD'\turdcode\t;; comment")
    }

    private def checkCorrectMacroInvocation(textLines: List[Label]): List[Line] = {
        val lines = parseLines(textLines)
        val stmts = lines.map {
            _.stmt.get
        }
        stmts must be(List(
            MacroStart(new MacroName("$CODE"), List(new MacroParameterName("LEX"), new MacroParameterName("NAME"), new MacroParameterName("LABEL"))),
            MacroEnd(),
            MacroInvocation(new MacroName("$CODE"), List(new MacroArgument("3"), new MacroArgument("'URD'"), new MacroArgument("urdcode")))
        ))
        lines
    }

    @Test
    def fullMacroInvocation(): Unit = {
        parseCodeMacro

        val lines = parseLine("\t\t$CODE\t3,'?RX',QRX")
        dumpLines(lines)
        lines must be (List(
            Line(13, "$CODE\t3,'?RX',QRX", None, Some(MacroInvocation(new MacroName("$CODE"), List( new MacroArgument("3"), new MacroArgument("'?RX'"), new MacroArgument("QRX"))))),
            Line(13, "ALIGN\t4", None, Some(Align(4))),
            Line(13, "QRX:", Some(new Label("QRX")), None),
            Line(13, "_CODE\t= $", None, Some(VariableAssignment(new SymbolName("_CODE"), SymbolArg("$")))),
            Line(13, "_LEN\t= (3 AND 01FH)/CELLL", None, Some(VariableAssignment(new SymbolName("_LEN"), Binary(Div(), Binary(And(), Number(3), Number(31)), SymbolArg("CELLL"))))),
            Line(13, "_NAME\t= _NAME-((_LEN+3)*CELLL)", None, Some(VariableAssignment(new SymbolName("_NAME"), Binary(Sub(), SymbolArg("_NAME"), Binary(Mult(), Binary(Add(), SymbolArg("_LEN"), Number(3)), SymbolArg("CELLL")))))),
            Line(13, "ORG\t_NAME", None, Some(Org(SymbolArg("_NAME")))),
            Line(13, "DD\t _CODE,_LINK", None, Some(DD(List(SymbolArg("_CODE"), SymbolArg("_LINK"))))),
            Line(13, "_LINK\t= $", None, Some(VariableAssignment(new SymbolName("_LINK"), SymbolArg("$")))),
            Line(13, "DB\t3,'?RX'", None, Some(DB(List(Number(3), Characters("?RX"))))),
            Line(13, "ORG\t_CODE", None, Some(Org(SymbolArg("_CODE"))))
        ))
    }

    @Test
    def fullNestedMacroInvocation(): Unit = {
        parseCodeMacro
        parseColonMacro

        logger.debug("Parsing a $COLON invocation")
        val lines = parseLine("\t\t$COLON\tCOMPO+5,'doVAR',DOVAR")
        val expectedLines = List(
            Line(18, "$COLON\tCOMPO+5,'doVAR',DOVAR", None, Some(MacroInvocation(new MacroName("$COLON"), List( new MacroArgument("COMPO+5"), new MacroArgument("'doVAR'"), new MacroArgument("DOVAR"))))),
            Line(18, "$CODE\tCOMPO+5,'doVAR',DOVAR", None, Some(MacroInvocation(new MacroName("$CODE"), List( new MacroArgument("COMPO+5"), new MacroArgument("'doVAR'"), new MacroArgument("DOVAR"))))),
            Line(18, "ALIGN\t4", None, Some(Align(4))),
            Line(18, "DOVAR:", Some(new Label("DOVAR")), None),
            Line(18, "_CODE\t= $", None, Some(VariableAssignment(new SymbolName("_CODE"), SymbolArg("$")))),
            Line(18, "_LEN\t= (COMPO+5 AND 01FH)/CELLL", None, Some(VariableAssignment(new SymbolName("_LEN"), Binary(Div(), Binary(Add(), SymbolArg("COMPO"), Binary(And(), Number(5), Number(31))), SymbolArg("CELLL"))))), /* TODO wrong precedence? */
            Line(18, "_NAME\t= _NAME-((_LEN+3)*CELLL)", None, Some(VariableAssignment(new SymbolName("_NAME"), Binary(Sub(), SymbolArg("_NAME"), Binary(Mult(), Binary(Add(), SymbolArg("_LEN"), Number(3)), SymbolArg("CELLL")))))),
            Line(18, "ORG\t_NAME", None, Some(Org(SymbolArg("_NAME")))),
            Line(18, "DD\t _CODE,_LINK", None, Some(DD(List(SymbolArg("_CODE"), SymbolArg("_LINK"))))),
            Line(18, "_LINK\t= $", None, Some(VariableAssignment(new SymbolName("_LINK"), SymbolArg("$")))),
            Line(18, "DB\tCOMPO+5,'doVAR'", None, Some(DB(List(Binary(Add(), SymbolArg("COMPO"), Number(5)), Characters("doVAR"))))),
            Line(18, "ORG\t_CODE", None, Some(Org(SymbolArg("_CODE")))),
            Line(18, "align\t4", None, Some(Align(4))),
            Line(18, "db\t048h", None, Some(DB(List(Number(72)))))
        )
        lines must be (expectedLines)
    }

    private def parseCodeMacro = {
        logger.debug("Parsing the $CODE macro")
        parseLines(List(
            "$CODE\tMACRO\tLEX,NAME,LABEL",
            "\tALIGN\t4\t\t\t\t;;force to cell boundary",
            "LABEL:\t\t\t\t\t\t;;assembly label",
            "\t_CODE\t= $\t\t\t\t;;save code pointer",
            "\t_LEN\t= (LEX AND 01FH)/CELLL\t\t;;string cell count, round down",
            "\t_NAME\t= _NAME-((_LEN+3)*CELLL)\t;;new header on cell boundary",
            "ORG\t_NAME\t\t\t\t\t;;set name pointer",
            "\tDD\t _CODE,_LINK\t\t\t;;token pointer and link",
            "\t_LINK\t= $\t\t\t\t;;link points to a name string",
            "\tDB\tLEX,NAME\t\t\t;;name string",
            "ORG\t_CODE\t\t\t\t\t;;restore code pointer",
            "\tENDM"
        ))
    }

    private def parseColonMacro = {
        logger.debug("Parsing the $COLON macro")
        // This is a cut-down version of the full $COLON macro; the db list in the original isn't needed to test
        // that nested macros work.
        parseLines(List(
            "$COLON\tMACRO\tLEX,NAME,LABEL",
            "\t$CODE\tLEX,NAME,LABEL",
            "\talign\t4",
            "\tdb\t048h\t\t;; ldc x\tpoint to dd dolst",
            "\tENDM"
        ))
    }

    @Test
    def labelAtMacroInvocationOccursOnFirstLineOfExpansion(): Unit = {
        parseLines(List(
            "FOO\tMACRO\tFIRST,SECOND",
            "\tDB\tFIRST",
            "\tDB\tSECOND",
            "\tENDM"
        ))
        val lines = parseLine("LLL:\tFOO\t1,2")
        dumpLines(lines)
        lines must be (List(
            Line(5, "LLL:\tFOO\t1,2", Some(new Label("LLL")), Some(MacroInvocation(new MacroName("FOO"), List( new MacroArgument("1"), new MacroArgument("2"))))),
            Line(5, "DB\t1", Some(new Label("LLL")), Some(DB(List(Number(1))))),
            Line(5, "DB\t2", None, Some(DB(List(Number(2)))))
        ))
    }

    private def dumpLines(lines: List[Line]) = {
        logger.info("======================================")
        lines.foreach((l: Line) => logger.info(l.toString))
        logger.info("======================================")
    }

    @Test
    def labelInFirstMacroBodyLineOccursOnFirstLineOfExpansionIfNoLabelAtMacroInvocation(): Unit = {
        parseLines(List(
            "FOO\tMACRO\tFIRST,SECOND",
            "LLL:\tDB\tFIRST",
            "\tDB\tSECOND",
            "\tENDM"
        ))
        val lines = parseLine("FOO\t1,2")
        dumpLines(lines)
        lines must be (List(
            Line(5, "FOO\t1,2", None, Some(MacroInvocation(new MacroName("FOO"), List( new MacroArgument("1"), new MacroArgument("2"))))),
            Line(5, "LLL:\tDB\t1", Some(new Label("LLL")), Some(DB(List(Number(1))))),
            Line(5, "DB\t2", None, Some(DB(List(Number(2)))))
        ))
    }

    @Test
    def labelAtMacroInvocationTakesPrecedenceOverLabelInFirstMacroBodyLine(): Unit = {
        parseLines(List(
            "FOO\tMACRO\tFIRST,SECOND",
            "LLL:\tDB\tFIRST",
            "\tDB\tSECOND",
            "\tENDM"
        ))
        val lines = parseLine("XXX:\tFOO\t1,2")
        dumpLines(lines)
        lines must be (List(
            Line(5, "XXX:\tFOO\t1,2", Some(new Label("XXX")), Some(MacroInvocation(new MacroName("FOO"), List( new MacroArgument("1"), new MacroArgument("2"))))),
            Line(5, "LLL:\tDB\t1", Some(new Label("XXX")), Some(DB(List(Number(1))))), // Yes, the text says LLL: but the Line's Label is what matters.
            Line(5, "DB\t2", None, Some(DB(List(Number(2)))))
        ))
    }

    @Test
    def singleSemicolonCommentsArePassedThroughInMacroExpansions(): Unit = {
        parseLines(List(
            "FOO\tMACRO",
            "\t; single semicolon passed through",
            "\tENDM"
        ))
        val lines = parseLine("\tFOO")
        dumpLines(lines)
        lines must be (List(
            Line(4, "FOO", None, Some(MacroInvocation(new MacroName("FOO"), List()))),
            Line(4, "; single semicolon passed through", None, None)
        ))
    }

    @Test
    def doubleSemicolonCommentsAreNotPassedThroughInMacroExpansions(): Unit = {
        parseLines(List(
            "FOO\tMACRO",
            "\t  ;; double semicolon not passed through",
            "\tENDM"
        ))
        val lines = parseLine("\tFOO")
        dumpLines(lines)
        lines must be (List(
            Line(4, "FOO", None, Some(MacroInvocation(new MacroName("FOO"), List()))),
            Line(4, "", None, None)
        ))
    }

    @Test
    def singleSemicolonCommentsArePassedThroughInMacroInvocations(): Unit = {
        parseLines(List(
            "FOO\tMACRO",
            "\tENDM"
        ))
        val lines = parseLine("\tFOO\t; comment")
        dumpLines(lines)
        lines must be (List(
            Line(3, "FOO\t; comment", None, Some(MacroInvocation(new MacroName("FOO"), List())))
        ))
    }

    @Test
    def doubleSemicolonCommentsArePassedThroughInMacroInvocations(): Unit = {
        parseLines(List(
            "FOO\tMACRO",
            "\tENDM"
        ))
        val lines = parseLine("\tFOO\t;; comment")
        dumpLines(lines)
        lines must be (List(
            Line(3, "FOO\t;; comment", None, Some(MacroInvocation(new MacroName("FOO"), List())))
        ))
    }

    @Test
    def doubleSemicolonCommentsInSingleQuotedStringsArePassedThroughInMacroExpansions(): Unit = {
        parseLines(List(
            "FOO\tMACRO",
            "\tDB ';; see me'",
            "\tENDM"
        ))
        val lines = parseLine("\tFOO")
        dumpLines(lines)
        lines must be (List(
            Line(4, "FOO", None, Some(MacroInvocation(new MacroName("FOO"), List()))),
            Line(4, "DB ';; see me'", None, Some(DB(List(Characters(";; see me")))))
        ))
    }

    @Test
    def doubleSemicolonCommentsInDoubleQuotedStringsArePassedThroughInMacroExpansions(): Unit = {
        parseLines(List(
            "FOO\tMACRO",
            "\tDB \";; see me\"",
            "\tENDM"
        ))
        val lines = parseLine("\tFOO")
        dumpLines(lines)
        lines must be (List(
            Line(4, "FOO", None, Some(MacroInvocation(new MacroName("FOO"), List()))),
            Line(4, "DB \";; see me\"", None, Some(DB(List(Characters(";; see me")))))
        ))
    }

}
