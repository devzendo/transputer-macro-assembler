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

package org.devzendo.tma.codegen

import org.devzendo.tma.ast.AST.SymbolName
import org.devzendo.tma.ast._
import org.junit.Test
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

// Converge mode processing
class TestCodeGeneratorConvergence extends CodeGeneratorFixture with AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger

    @Test
    def convergeModeExample1(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tLDC L1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L1")))),
            Line(3, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(4, "\tDB\t255 DUP 10", None, Some(DBDup(Number(255), Number(10)))), // pad the LDC out to 3 bytes
            Line(5, "L1:\tDB\t'hello world'", Some("L1"), Some(DB(List(Characters("hello world")))))
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x10F)

        model.getLabel(CasedSymbolName("L1")) must be(0x104)

        // The LDC is encoded with the right size for the offest of L1 - there have been iterations to increase its
        // size from its initial length of 1 byte.
        val line2Storages = model.getSourcedValuesForLineNumber(2)
        line2Storages must have size 1
        val line2Storage = singleStorage(line2Storages)
        line2Storage.address must be(0)
        line2Storage.cellWidth must be(1)
        line2Storage.data.toList must be(List(0x21, 0x20, 0x44))

        // Statements that are not DirectInstructions (here's an IndirectInstruction, the LDPI)
        val line3Storages = model.getSourcedValuesForLineNumber(3)
        line3Storages must have size 1
        val line3Storage = singleStorage(line3Storages)
        line3Storage.address must be(3)
        line3Storage.cellWidth must be(1)
        line3Storage.data.toList must be(List(0x21, 0xfb))

        // Each line (that generates storage) only generates one for the whole convergence - storages generated in
        // early iterations are removed.
        val line5Storages = model.getSourcedValuesForLineNumber(5) // the DB hello world
        line5Storages.size must be (2) // a storage and a label
        val line5Storage = singleStorage(line5Storages)
        line5Storage.address must be(0x104)
        line5Storage.cellWidth must be(1)
        line5Storage.data.toList must be(List(0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64))
        val line5ValueAssignment = singleAssignmentValue(line5Storages) // L1, see above for label test
        line5ValueAssignment.data must be(0x104)
    }

    @Test
    def convergeModeExample2(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tLDC L1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L1")))),
            Line(3, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(4, "\tLDC L2", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L2")))),
            Line(5, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(6, "L1:\tDB\t'hello'", Some("L1"), Some(DB(List(Characters("hello"))))),
            Line(7, "L2:\tDB\t'again'", Some("L2"), Some(DB(List(Characters("again")))))
        )
        val model = generateFromLines(lines)
        showListing(model)

        val line2Storages = model.getSourcedValuesForLineNumber(2)
        line2Storages must have size 1
        val line2Storage = singleStorage(line2Storages)
        line2Storage.address must be(0)
        line2Storage.cellWidth must be(1)
        line2Storage.data.toList must be(List(0x46)) // Initial single byte for LDC has not needed expanding since it fits
    }

    @Test
    def convergeModeExample3(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "L2:\tDB\t'again'", Some("L2"), Some(DB(List(Characters("again"))))),
            Line(3, "\tLDC L1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L1")))),
            Line(4, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(5, "\tCALL PRINTF", None, Some(DirectInstruction("CALL", 0x90, SymbolArg("PRINTF")))),
            Line(6, "\tLDC L2", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L2")))),
            Line(7, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(8, "L1:\tDB\t'hello'", Some("L1"), Some(DB(List(Characters("hello"))))),
            Line(9, "PRINTF:", Some("PRINTF"), None)
        )
        val model = generateFromLines(lines)
        showListing(model)
    }

    @Test
    def convergeModeExample4(): Unit = {
        // lines needing convergence are in the middle section
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tAJW 6", None, Some(DirectInstruction("AJW", 0xb0, Number(6)))), // nonsense program
            Line(3, "\tRESETCH", None, Some(IndirectInstruction("RESETCH", List(0x21, 0xf2)))),
            Line(4, "\tfpusqrtfirst", None, Some(IndirectInstruction("FPUSQRTFIRST", List(0x41, 0x2a, 0xfb)))),
            Line(5, "\tJ PRINTF", None, Some(DirectInstruction("J", 0x00, SymbolArg("PRINTF")))), // START CONVERGENCE
            Line(6, "L2:\tDB\t'again'", Some("L2"), Some(DB(List(Characters("again"))))),
            Line(7, "\tLDC L1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L1")))),
            Line(8, "\tCALL PRINTF", None, Some(DirectInstruction("CALL", 0x90, SymbolArg("PRINTF")))),
            Line(9, "\tLDC L2", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L2")))),
            Line(10, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(11, "L1:\tDB\t'hello'", Some("L1"), Some(DB(List(Characters("hello"))))),
            Line(12, "PRINTF:", Some("PRINTF"), None), // END CONVERGENCE
            Line(13, "\tfpusqrtfirst", None, Some(IndirectInstruction("FPUSQRTFIRST", List(0x41, 0x2a, 0xfb))))
        )
        val model = generateFromLines(lines)
        showListing(model)
    }

    @Test
    def convergedAdjustedLabelsCauseUpdateToStorage(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tORG 0x1000", None, Some(Org(Number(0x1000)))),
            Line(3, "\tDD L1", None, Some(DD(List(SymbolArg("L1"))))), // Is this storage updated when L1 is known?
            Line(4, "\tLDC L1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L1")))),
            Line(5, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(6, "\tDB\t255 DUP 10", None, Some(DBDup(Number(255), Number(10)))), // pad the LDC out to 3 bytes
            Line(7, "L1:\tDB\t'hello world'", Some("L1"), Some(DB(List(Characters("hello world")))))
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x1114)

        val expectedL1 = 0x1109
        model.getLabel(CasedSymbolName("L1")) must be(expectedL1)

        // What is that DD now set to?
        val line3Storages = model.getSourcedValuesForLineNumber(3)
        line3Storages must have size 1
        val line3Storage = singleStorage(line3Storages)
        line3Storage.address must be(0x1000)
        line3Storage.cellWidth must be(4)
        line3Storage.data.toList must be(List(expectedL1))
    }

    @Test
    def convergedAdjustedConstantsCauseUpdateToStorage(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tORG 0x1000", None, Some(Org(Number(0x1000)))),
            Line(3, "\tDD C1", None, Some(DD(List(SymbolArg("C1"))))), // Is this storage updated when C1 is known?
            Line(4, "\tLDC C1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("C1")))),
            Line(5, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(6, "\tDB\t255 DUP 10", None, Some(DBDup(Number(255), Number(10)))), // pad the LDC out to 3 bytes
            Line(7, "C1\tEQU\t$", None, Some(ConstantAssignment(new SymbolName("C1"), SymbolArg("$"))))
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x1109)

        val expectedC1 = 0x1109
        model.getConstant(CasedSymbolName("C1")) must be(expectedC1)

        // What is that DD now set to?
        val line3Storages = model.getSourcedValuesForLineNumber(3)
        line3Storages must have size 1
        val line3Storage = singleStorage(line3Storages)
        line3Storage.address must be(0x1000)
        line3Storage.cellWidth must be(4)
        line3Storage.data.toList must be(List(expectedC1))
    }

    @Test
    def convergedAdjustedVariablesCauseUpdateToStorage(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tORG 0x1000", None, Some(Org(Number(0x1000)))),
            Line(3, "\tDD V1", None, Some(DD(List(SymbolArg("V1"))))), // Is this storage updated when V1 is known?
            Line(4, "\tLDC V1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("V1")))),
            Line(5, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(6, "\tDB\t255 DUP 10", None, Some(DBDup(Number(255), Number(10)))), // pad the LDC out to 3 bytes
            Line(7, "V1\t=\t$", None, Some(ConstantAssignment(new SymbolName("V1"), SymbolArg("$"))))
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x1109)

        val expectedV1 = 0x1109
        model.getConstant(CasedSymbolName("V1")) must be(expectedV1)

        // What is that DD now set to?
        val line3Storages = model.getSourcedValuesForLineNumber(3)
        line3Storages must have size 1
        val line3Storage = singleStorage(line3Storages)
        line3Storage.address must be(0x1000)
        line3Storage.cellWidth must be(4)
        line3Storage.data.toList must be(List(expectedV1))
    }

    @Test
    def convergedAdjustedLabelsCauseUpdateToConstants(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tORG 0x1000", None, Some(Org(Number(0x1000)))),
            Line(3, "\tL1COPY EQU L1", None, Some(ConstantAssignment("L1COPY", SymbolArg("L1")))), // Is this constant updated when L1 is known?
            Line(4, "\tLDC L1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L1")))),
            Line(5, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(6, "\tDB\t255 DUP 10", None, Some(DBDup(Number(255), Number(10)))), // pad the LDC out to 3 bytes
            Line(7, "L1:\tDB\t'hello world'", Some("L1"), Some(DB(List(Characters("hello world"))))),
            Line(8, "\tDD\tL1COPY", None, Some(DD(List(SymbolArg("L1COPY")))))
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x1114)

        val expectedL1 = 0x1105
        model.getLabel(CasedSymbolName("L1")) must be(expectedL1)
        model.getConstant(CasedSymbolName("L1COPY")) must be(expectedL1)

        // What is that AssignedValue now set to? There will be several, what's in the last (which will be what's in
        // the binary file)
        val line3SourcedValues = model.getSourcedValuesForLineNumber(3)
        line3SourcedValues.foreach( (sv: SourcedValue) => logger.debug(s"L1COPY sourced value $sv"))
        line3SourcedValues must have size 5
        val line3AssignmentValue = lastAssignmentValue(line3SourcedValues)
        line3AssignmentValue.data must be(expectedL1)

        // What does L1COPY get stored as in the DD on line 8?
        val line8SourcedValues = model.getSourcedValuesForLineNumber(8)
        line8SourcedValues.foreach( (sv: SourcedValue) => logger.debug(s"DD L1COPY sourced value $sv"))
        line8SourcedValues must have size 1
        val line8Storage = singleStorage(line8SourcedValues)
        line8Storage.data(0) must be(expectedL1)
    }

    @Test
    def convergedAdjustedLabelsCauseUpdateToVariablesOnFirstSetting(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tORG 0x1000", None, Some(Org(Number(0x1000)))),
            Line(3, "\tL1COPY = L1", None, Some(VariableAssignment("L1COPY", SymbolArg("L1")))), // Is this variable updated when L1 is known?
            Line(4, "\tLDC L1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L1")))),
            Line(5, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(6, "\tDB\t255 DUP 10", None, Some(DBDup(Number(255), Number(10)))), // pad the LDC out to 3 bytes
            Line(7, "L1:\tDB\t'hello world'", Some("L1"), Some(DB(List(Characters("hello world")))))
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x1110)

        val expectedL1 = 0x1105
        model.getLabel(CasedSymbolName("L1")) must be(expectedL1)

        val firstL1 = 0x1102
        model.getVariable(CasedSymbolName("L1COPY")) must be(firstL1)

        // What is that AssignedValue now set to?
        val line3SourcedValues = model.getSourcedValuesForLineNumber(3)
        line3SourcedValues.foreach( (sv: SourcedValue) => logger.debug(s"L1COPY sourced value $sv"))
        line3SourcedValues must have size 1
        val line3AssignmentValue = singleAssignmentValue(line3SourcedValues)
        line3AssignmentValue.data must be(firstL1)
    }

    @Test
    def convergedAdjustedLabelsCauseUpdateToVariablesAndReferencesToTheseAreUpdatedOnFirstSetting(): Unit = {
        val lines = List(
            Line(1, "\t.TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(2, "\tORG 0x1000", None, Some(Org(Number(0x1000)))),
            Line(3, "\tL1COPY = L1", None, Some(VariableAssignment("L1COPY", SymbolArg("L1")))),
            Line(4, "\tLDC L1", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("L1")))),
            Line(5, "\tLDPI", None, Some(IndirectInstruction("LDPI", List(0x21, 0xfb)))),
            Line(6, "\tDB\t255 DUP 10", None, Some(DBDup(Number(255), Number(10)))), // pad the LDC out to 3 bytes
            Line(7, "L1:\tDB\t'hello world'", Some("L1"), Some(DB(List(Characters("hello world"))))),
            Line(8, "\tDD L1COPY", None, Some(DD(List(SymbolArg("L1COPY"))))) // Is this storage updated when L1COPY is known? (after L1 is known?)
        )
        val model = generateFromLines(lines)
        model.convergeMode must be(false)
        showListing(model)

        model.getDollar must be(0x1114)

        val expectedL1 = 0x1105
        model.getLabel(CasedSymbolName("L1")) must be(expectedL1)

        val firstL1 = 0x1102
        model.getVariable(CasedSymbolName("L1COPY")) must be(firstL1)

        // What is that DD now set to?
        val line8Storages = model.getSourcedValuesForLineNumber(8)
        line8Storages must have size 1
        val line8Storage = singleStorage(line8Storages)
        line8Storage.address must be(0x1110)
        line8Storage.cellWidth must be(4)
        line8Storage.data.toList must be(List(firstL1))
    }
}
