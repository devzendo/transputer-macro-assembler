/*
 * Copyright (C) 2008-2020 Matt Gumbley, DevZendo.org http://devzendo.org
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

package org.devzendo.tma.codegen

import org.devzendo.tma.ast.AST.{Label, SymbolName}
import org.devzendo.tma.ast._
import org.devzendo.tma.output.ShowListingFixture
import org.devzendo.tma.{AssemblerFixture, TransputerDirectInstructions}
import org.junit.rules.ExpectedException
import org.junit.{Before, Rule, Test}
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestOffsetTransformer extends CodeGeneratorFixture with AssemblerFixture with SourcedValuesFixture with ShowListingFixture with TransputerDirectInstructions with AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger

    val fnord = "FNORD"

    val transform = new OffsetTransformer(model).transform _

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    @Before
    def addOffsetTransformer(): Unit = {
        codegen.addStatementTransformer(transform)
    }

    @Test
    def convertRepeatedOffsetsCountUndefined(): Unit = {
        thrown.expect(classOf[StatementTransformationException])
        thrown.expectMessage("Count of 'SymbolArg(FNORD)' is undefined")

        val count = SymbolArg(fnord)
        val repeatedExpr = Number(7)

        val inmodel = new AssemblyModel(true)
        new OffsetTransformer(inmodel).convertRepeatedOffsets(count, repeatedExpr, 1)
    }

    @Test
    def offsetConversionToOffsetFrom(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)
        val convertedExpr = new OffsetTransformer(inmodel).convertOffsets(Unary(Offset(), Number(0x10)))
        convertedExpr must be(Unary(OffsetFrom(0x1000), Number(0x10)))
    }

    @Test
    def nonOffsetExpressionInUnaryDoesNotGetConverted(): Unit = {
        val inmodel = new AssemblyModel(true)

        val convertedExpr = new OffsetTransformer(inmodel).convertOffsets(Unary(Negate(), Number(0x10)))
        convertedExpr must be(Unary(Negate(), Number(0x10)))
    }

    @Test
    def nonOffsetExpressionInNonUnaryDoesNotGetConverted(): Unit = {
        val inmodel = new AssemblyModel(true)

        val convertedExpr = new OffsetTransformer(inmodel).convertOffsets(Characters("foo"))
        convertedExpr must be(Characters("foo"))
    }

    @Test
    def listOfOffsetBytesGetIncreasingDollar(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)

        val convertedExpr = new OffsetTransformer(inmodel).convertListOfOffsets(List(
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10))), 1)
        convertedExpr must be(List(
            Unary(OffsetFrom(0x1000), Number(0x10)),
            Unary(OffsetFrom(0x1001), Number(0x10)),
            Unary(OffsetFrom(0x1002), Number(0x10))))
    }

    @Test
    def listOfOffsetWordsGetIncreasingDollar(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)

        val convertedExpr = new OffsetTransformer(inmodel).convertListOfOffsets(List(
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10))), 2)
        convertedExpr must be(List(
            Unary(OffsetFrom(0x1000), Number(0x10)),
            Unary(OffsetFrom(0x1002), Number(0x10)),
            Unary(OffsetFrom(0x1004), Number(0x10))))
    }

    @Test
    def listOfOffsetDoubleWordsGetIncreasingDollar(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)

        val convertedExpr = new OffsetTransformer(inmodel).convertListOfOffsets(List(
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10))), 4)
        convertedExpr must be(List(
            Unary(OffsetFrom(0x1000), Number(0x10)),
            Unary(OffsetFrom(0x1004), Number(0x10)),
            Unary(OffsetFrom(0x1008), Number(0x10))))
    }

    @Test
    def listOfOffsetsInterspersedWithConstantsGetIncreasingDollar(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)

        val convertedExpr = new OffsetTransformer(inmodel).convertListOfOffsets(List(
            Unary(Offset(), Number(0x10)),
            Number(0x10),
            Unary(Offset(), Number(0x10))), 4)
        convertedExpr must be(List(
            Unary(OffsetFrom(0x1000), Number(0x10)),
            Number(0x10),
            Unary(OffsetFrom(0x1008), Number(0x10))))
    }

    @Test
    def offsetInOrg(): Unit = {
        // bit odd, but why not?!
        val model = generateFromStatements(List(
            Org(Number(4)),
            Org(Unary(Offset(), Number(12))) // 12 - 4
        ))
        model.getDollar must be(8)
    }

    @Test
    def offsetInConstant(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            ConstantAssignment(new SymbolName(fnord), Unary(Offset(), Number(12))) // 12 - 4
        ))
        model.getConstant(CasedSymbolName(fnord)) must be(8)
    }

    @Test
    def offsetInVariable(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            VariableAssignment(new SymbolName(fnord), Unary(Offset(), Number(12))) // 12 - 4
        ))
        model.getVariable(CasedSymbolName(fnord)) must be(8)
    }

    @Test
    def offsetInDB(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DB(List(
                Unary(Offset(), Number(12)),
                Unary(Offset(), Number(12)),
                Unary(Offset(), Number(12)),
            ))
        ))
        val cellWidth = 1
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](
                12 - (4 + (0*cellWidth)),
                12 - (4 + (1*cellWidth)),
                12 - (4 + (2*cellWidth))
            ))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDW(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DW(List(
                Unary(Offset(), Number(12)),
                Unary(Offset(), Number(12)),
                Unary(Offset(), Number(12))
            ))
        ))
        val cellWidth = 2
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](
                12 - (4 + (0*cellWidth)),
                12 - (4 + (1*cellWidth)),
                12 - (4 + (2*cellWidth))
            ))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDD(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DD(List(
                Unary(Offset(), Number(12)),
                Unary(Offset(), Number(12)),
                Unary(Offset(), Number(12))
            ))
        ))
        val cellWidth = 4
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](
                12 - (4 + (0*cellWidth)),
                12 - (4 + (1*cellWidth)),
                12 - (4 + (2*cellWidth))
            ))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDirectInstructionExpression(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DirectInstruction("LDC", 0x40, Unary(Offset(), Number(12))) // 12 - 4 (but the end of instruction is 5)
        ))
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](0x47)) // 12 - 5
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDBDupCount(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DBDup(Unary(Offset(), Number(12)), Number(7)) // 12 - 4
        ))
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](7, 7, 7, 7, 7, 7, 7, 7))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDBDupRepeatedExpression(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DBDup(Number(3), Unary(Offset(), Number(12)))
        ))
        val cellWidth = 1
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](
                12 - (4 + (0*cellWidth)),
                12 - (4 + (1*cellWidth)),
                12 - (4 + (2*cellWidth))
            ))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDWDupCount(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DWDup(Unary(Offset(), Number(12)), Number(7)) // 12 - 4
        ))
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](7, 7, 7, 7, 7, 7, 7, 7))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDWDupRepeatedExpression(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DWDup(Number(3), Unary(Offset(), Number(12)))
        ))
        val cellWidth = 2
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](
                12 - (4 + (0*cellWidth)),
                12 - (4 + (1*cellWidth)),
                12 - (4 + (2*cellWidth))
            ))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDDDupCount(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DDDup(Unary(Offset(), Number(12)), Number(7)) // 12 - 4
        ))
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](7, 7, 7, 7, 7, 7, 7, 7))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def offsetInDDDupRepeatedExpression(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            DDDup(Number(3), Unary(Offset(), Number(12))) // 12 - 4
        ))
        val cellWidth = 4
        model.getSourcedValuesForLineNumber(2).head match {
            case Storage(4, _, data, _, _) => data must be(Array[Int](
                12 - (4 + (0*cellWidth)),
                12 - (4 + (1*cellWidth)),
                12 - (4 + (2*cellWidth))
            ))
            case _ => fail("Did not return a Storage")
        }
    }

    @Test
    def dbDupNumbers(): Unit = {
        val cellWidth = 1
        val count = 5
        val dbDupStatement = DBDup(Number(count), Number(69))
        val line = Line(1, "", None, Some(dbDupStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineNumber(1)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(69, 69, 69, 69, 69))
        storage.line must be(Line(1, "", None, Some(DB(List(Number(69), Number(69), Number(69), Number(69), Number(69))))))
        model.getDollar must be(0 + (cellWidth * count))
    }

    @Test
    def dbDupZeroCount(): Unit = {
        // It's allowed to have a dbdup with 0 length (as a constant or number) - you wouldn't do this, but no reason
        // to disallow it.
        val count = 0
        val dbDupStatement = DBDup(Number(count), Number(69))
        val line = Line(1, "", None, Some(dbDupStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineNumber(1)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List.empty)
        storage.line must be(Line(1, "", None, Some(DB(List()))))
        model.getDollar must be(0)
    }

    @Test
    def negativeOffset(): Unit = {
        val lines = List(
            Line(1, "LABEL: DB 1,2,3,4", Some("LABEL"), Some(DB(List(Number(1), Number(2), Number(3), Number(4))))),
            Line(2, "C EQU OFFSET LABEL'", None, Some(ConstantAssignment(new SymbolName("C"), Unary(Offset(), SymbolArg(new SymbolName("LABEL"))))))
        )
        val model = generateFromLines(lines)

        model.getConstant(CasedSymbolName("C")) must be(-4)
    }

    @Test
    def zeroOffset(): Unit = {
        val lines = List(
            Line(1, "LABEL:", Some("LABEL"), None),
            Line(2, "C EQU OFFSET LABEL'", None, Some(ConstantAssignment(new SymbolName("C"), Unary(Offset(), SymbolArg(new SymbolName("LABEL"))))))
        )
        val model = generateFromLines(lines)

        model.getConstant(CasedSymbolName("C")) must be(0)
    }

    @Test
    def positiveOffset(): Unit = {
        val lines = List(
            Line(1, "C EQU OFFSET LABEL'", None, Some(ConstantAssignment(new SymbolName("C"), Unary(Offset(), SymbolArg(new SymbolName("LABEL")))))),
            Line(2, "DB 1,2,3,4", None, Some(DB(List(Number(1), Number(2), Number(3), Number(4))))),
            Line(3, "LABEL:", Some("LABEL"), None)
        )
        val model = generateFromLines(lines)

        model.getConstant(CasedSymbolName("C")) must be(4)
    }

    @Test
    def convergeOffset(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tORG 0x1000", None, Some(Org(Number(0x1000)))),
            Line(3, "\tLDC OFFSET L1", None, Some(DirectInstruction("LDC", 0x40, Unary(Offset(), SymbolArg("L1"))))),
            Line(4, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(5, "\tDB\t255 DUP 10", None, Some(DBDup(Number(255), Number(10)))), // pad the LDC out to 3 bytes
            Line(6, "L1:\tDB\t'hello world'", Some("L1"), Some(DB(List(Characters("hello world")))))
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x110F)

        model.getLabel(CasedSymbolName("L1")) must be(0x1104)

        // The LDC is encoded with the right size for the offset of L1 - there have been iterations to increase its
        // size from its initial length of 1 byte.
        val line3Storages = model.getSourcedValuesForLineNumber(3)
        line3Storages must have size 1
        val line3Storage = singleStorage(line3Storages)
        line3Storage.address must be(0x1000)
        line3Storage.cellWidth must be(1)
        line3Storage.data.toList must be(List(0x21, 0x20, 0x41))

        // Statements that are not DirectInstructions (here's an IndirectInstruction, the LDPI)
        val line4Storages = model.getSourcedValuesForLineNumber(4)
        line4Storages must have size 1
        val line4Storage = singleStorage(line4Storages)
        line4Storage.address must be(0x1003)
        line4Storage.cellWidth must be(1)
        line4Storage.data.toList must be(List(0x21, 0xfb))

        // Each line (that generates storage) only generates one for the whole convergence - storages generated in
        // early iterations are removed.
        val line6Storages = model.getSourcedValuesForLineNumber(6) // the DB hello world
        line6Storages.size must be (2) // a storage and a label
        val line6Storage = singleStorage(line6Storages)
        line6Storage.address must be(0x1104)
        line6Storage.cellWidth must be(1)
        line6Storage.data.toList must be(List(0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64))
        val line6ValueAssignment = singleAssignmentValue(line6Storages) // L1, see above for label test
        line6ValueAssignment.data must be(0x1104)
    }

    @Test
    def offsetsInDataSequencesAreRelativeToTheirPosition(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tORG 0x1000", None, Some(Org(Number(0x1000)))),
            Line(3, "\tDD OFFSET X, OFFSET X, OFFSET X", None, Some(DD(List(                                    // 3,2,1
                Unary(Offset(), SymbolArg("X")),
                Unary(Offset(), SymbolArg("X")),
                Unary(Offset(), SymbolArg("X")))))),
            Line(4, "\tX: DD OFFSET X", Some(new Label("X")), Some(DD(List(Unary(Offset(), SymbolArg("X")))))), // 0
            Line(5, "\tDD OFFSET X, OFFSET X, OFFSET X", None, Some(DD(List(                                    // -1,-2,-3
                Unary(Offset(), SymbolArg("X")),
                Unary(Offset(), SymbolArg("X")),
                Unary(Offset(), SymbolArg("X")))))),
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x101C)

        model.getLabel(CasedSymbolName("X")) must be(0x100C)

        // Positive offsets from X
        val line3SVs = model.getSourcedValuesForLineNumber(3)
        line3SVs must have size 1
        val line3Storage = singleStorage(line3SVs)
        line3Storage.address must be(0x1000)
        line3Storage.cellWidth must be(4)
        line3Storage.data.toList must be(List(3 * 4, 2 * 4, 1 * 4)) // *4's here illustrate these are DDs

        // X: DD OFFSET X has no offset!
        val line4SVs = model.getSourcedValuesForLineNumber(4)
        line4SVs must have size 2
        val line4Storage = singleStorage(line4SVs)
        line4Storage.address must be(0x100C)
        line4Storage.cellWidth must be(4)
        line4Storage.data.toList must be(List(0 * 4))
        val line4AssignmentValue = singleAssignmentValue(line4SVs)
        line4AssignmentValue.data must be(0x100C)

        // Positive offsets from X
        val line5SVs = model.getSourcedValuesForLineNumber(5)
        line5SVs must have size 1
        val line5Storage = singleStorage(line5SVs)
        line5Storage.address must be(0x1010)
        line5Storage.cellWidth must be(4)
        line5Storage.data.toList must be(List(-1 * 4, -2 * 4, -3 * 4))
    }

    @Test
    def transformJSymbolArgIntoOffset(): Unit = {
        model.setDollarSilently(0x1000)
        val j = DirectInstruction("J", 0x00, SymbolArg("LABEL"))
        transform(j) must be (DirectInstruction("J", 0x00, Unary(OffsetFrom(0x1000), SymbolArg("LABEL"))))
    }

    @Test
    def transformCJSymbolArgIntoOffset(): Unit = {
        model.setDollarSilently(0x1000)
        val cj = DirectInstruction("CJ", 0xA0, SymbolArg("LABEL"))
        transform(cj) must be (DirectInstruction("CJ", 0xA0, Unary(OffsetFrom(0x1000), SymbolArg("LABEL"))))
    }

    @Test
    def transformCALLSymbolArgIntoOffset(): Unit = {
        model.setDollarSilently(0x1000)
        val call = DirectInstruction("CALL", 0x90, SymbolArg("LABEL"))
        transform(call) must be (DirectInstruction("CALL", 0x90, Unary(OffsetFrom(0x1000), SymbolArg("LABEL"))))
    }

    val nonTranslatedDirectInstructionOpCodes = Seq(OP_LDLP, OP_PFIX, OP_LDNL, OP_LDC, OP_LDNLP, OP_NFIX, OP_LDL, OP_ADC, OP_AJW, OP_EQC, OP_STL, OP_STNL, OP_OPR)

    // Only J, CJ and CALL have symbolic arguments converted automatically into OffsetFrom...
    @Test
    def transformOtherSymbolArgNotConverted(): Unit = {
        for (op <- nonTranslatedDirectInstructionOpCodes) {
            val di = DirectInstruction("irrelevant", op, SymbolArg("LABEL"))
            transform(di) must be(DirectInstruction("irrelevant", op, SymbolArg("LABEL")))
        }
    }

    // You can explicitly use OFFSET SymbolArg in a non-{J, CJ, CALL}.
    @Test
    def transformOtherOffsetSymbolArgConverted(): Unit = {
        model.setDollarSilently(0x1000)
        for (op <- nonTranslatedDirectInstructionOpCodes) {
            val di = DirectInstruction("irrelevant", op, Unary(Offset(), SymbolArg("LABEL")))
            transform(di) must be(DirectInstruction("irrelevant", op, Unary(OffsetFrom(0x1000), SymbolArg("LABEL"))))
        }
    }

    @Test
    def transformJNonSymbolArgNotConverted(): Unit = {
        model.setDollarSilently(0x1000)
        val j = DirectInstruction("J", 0x00, Number(5))
        transform(j) must be (DirectInstruction("J", 0x00, Number(5)))
    }

    @Test
    def transformCJNonSymbolArgNotConverted(): Unit = {
        model.setDollarSilently(0x1000)
        val cj = DirectInstruction("CJ", 0xA0, Number(5))
        transform(cj) must be (DirectInstruction("CJ", 0xA0, Number(5)))
    }

    @Test
    def transformCALLNonSymbolArgNotConverted(): Unit = {
        model.setDollarSilently(0x1000)
        val call = DirectInstruction("CALL", 0x90, Number(5))
        transform(call) must be (DirectInstruction("CALL", 0x90, Number(5)))
    }

    @Test
    def transformOtherNonSymbolArgNotConverted(): Unit = {
        for (op <- nonTranslatedDirectInstructionOpCodes) {
            val di = DirectInstruction("irrelevant", op, Number(5))
            transform(di) must be(DirectInstruction("irrelevant", op, Number(5)))
        }
    }
}
