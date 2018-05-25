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

import org.devzendo.tma.ast.AST.{Label, MacroName, SymbolName}
import org.devzendo.tma.ast.{Line, _}
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestCodeGenerator extends AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger
    val dollar = "$"
    val fnord = "FNORD"

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val codegen = new CodeGenerator(true)

    @Test
    def initialConditions(): Unit = {
        codegen.getLastLineNumber must be (0)
        val model = codegen.createModel(List(Line(1, "", None, Some(End(None)))))
        model.title must be("")
        model.rows must be(25)
        model.columns must be(80)
        model.processor must be(None)
        model.getDollar must be(0)
        model.getVariable(dollar) must be (0)
        model.hasEndBeenSeen must be (true) // cannot sense it being false initially after model created
        codegen.getLastLineNumber must be(1)
    }

    private def generateFromStatements(stmts: List[Statement]): AssemblyModel = {
        val stmts2Lines = stmts.zipWithIndex.map((p: (Statement, Int)) => Line(p._2 + 1, p._1.toString, None, Some(p._1)))
        generateFromLines(stmts2Lines)
    }

    private def generateFromStatement(stmt: Statement): AssemblyModel = {
        generateFromLine(Line(1, "", None, Some(stmt)))
    }

    private def generateFromLine(line: Line): AssemblyModel = {
        generateFromLines(List(line))
    }

    private def generateFromLines(lines: List[Line]): AssemblyModel = {
        codegen.createModel(lines)
    }

    @Test
    def title(): Unit = {
        generateFromStatement(Title("custom title")).title must be("custom title")
    }

    @Test
    def page(): Unit = {
        val model = generateFromStatement(Page(60, 132))
        model.rows must be(60)
        model.columns must be(132)
    }

    @Test
    def processorT800(): Unit = {
        val model = generateFromStatement(Processor("T800"))
        model.processor must be(Some("T800"))
        model.endianness must be(Endianness.Little)
    }

    @Test
    def processor386(): Unit = {
        val model = generateFromStatement(Processor("386"))
        model.processor must be(Some("386"))
        model.endianness must be(Endianness.Little)
    }

    @Test
    def alignmentAlreadyAligned(): Unit = {
        val model = generateFromStatements(List(
            Org(Number(4)),
            Align(4)
        ))
        model.getDollar must be(4)
    }

    @Test
    def alignmentAdjusts(): Unit = {
        for (i <- 1 to 7) {
            logger.debug("Aligning address of " + i + " to 8 bytes")
            val localCodeGen = new CodeGenerator(true)
            val model = localCodeGen.createModel(List(
                Line(1, "", None, Some(Org(Number(i)))),
                Line(2, "", None, Some(Align(8))),
                Line(3, "", None, Some(End(None)))
            ))
            model.getDollar must be(8)
        }
    }

    @Test
    def end(): Unit = {
        val model = generateFromLines(List(Line(1, "", None, Some(End(None)))))
        codegen.endCheck()
        // Gets to the end, does not throw!
        model.hasEndBeenSeen must be (true)
    }

    @Test
    def endMissing(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("1: End of input reached with no End statement")

        generateFromLines(List(Line(1, "", None, None)))
        codegen.endCheck()
    }

    @Test
    def endWithSubsequentCode(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("2: No statements allowed after End statement")

        generateFromLines(List(
            Line(1, "", None, Some(End(None))),
            Line(2, "", None, Some(DB(List(Number(5)))))
        ))
        // no need to call endCheck, code gen will fail before then
    }

    @Test
    def ignored(): Unit = {
        generateFromStatement(Ignored())
        // nothing to test, it just doesn't throw...
    }

    @Test
    def orgUnknownSymbolRetrieval(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("1: Undefined symbol(s) 'FNORD'") // could be Constant, Variable or Label - don't know which

        generateFromStatement(Org(SymbolArg("FNORD")))
    }

    @Test
    def orgCharacterExpressionFails(): Unit = {
        // Parser allows any expression, which could be characters. Disallow it at code gen.
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("1: Origin cannot be set to a Character expression 'Characters(FNORD)'")
        generateFromStatement(Org(Characters("FNORD")))
    }

    @Test
    def orgNumber(): Unit = {
        val model = generateFromStatement(Org(Number(42)))
        model.getDollar must be(42)
        model.getVariable(dollar) must be(42)
    }

    @Test
    def orgDefinedVariable(): Unit = {
        val model = generateFromStatements(List(
            ConstantAssignment(new SymbolName(fnord), Number(42)),
            Org(SymbolArg(fnord))
        ))
        model.getDollar must be(42)
        model.getVariable(dollar) must be(42)
    }

    @Test
    def constantAssignment(): Unit = {
        val model = generateFromStatement(ConstantAssignment(new SymbolName(fnord), Number(42)))
        model.getConstant(fnord) must be(42)
    }

    @Test
    def constantAssignmentToCharacterExpressionFails(): Unit = {
        // Parser allows any expression, which could be characters. Disallow it at code gen.
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("1: Constant cannot be set to a Character expression 'Characters(FNORD)'")
        generateFromStatement(ConstantAssignment(new SymbolName(fnord),Characters("FNORD")))
    }

    @Test
    def constantAssignmentToUndefinedSymbolFails(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("1: Constant cannot be set to an undefined symbol 'Set(FNORD)'")
        generateFromStatement(ConstantAssignment(new SymbolName(fnord),SymbolArg(fnord)))
    }

    @Test
    def variableAssignment(): Unit = {
        val model = generateFromStatement(VariableAssignment(new SymbolName(fnord), Number(42)))
        model.getVariable(fnord) must be(42)
    }

    @Test
    def variableAssignmentToCharacterExpressionFails(): Unit = {
        // Parser allows any expression, which could be characters. Disallow it at code gen.
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("1: Variable cannot be set to a Character expression 'Characters(FNORD)'")
        generateFromStatement(VariableAssignment(new SymbolName(fnord),Characters("FNORD")))
    }

    @Test
    def variableAssignmentToUndefinedSymbolFails(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("1: Variable cannot be set to an undefined symbol 'Set(FNORD)'")
        generateFromStatement(VariableAssignment(new SymbolName(fnord),SymbolArg(fnord)))
    }

    @Test
    def variableReassignmentAllowed(): Unit = {
        val model = generateFromStatements(List(
            VariableAssignment(new SymbolName(fnord), Number(42)),
            VariableAssignment(new SymbolName(fnord), Number(12))
        ))
        model.getVariable(fnord) must be(12)
    }

    @Test
    def labelAssignmentFromDollar(): Unit = {
        val model = generateFromLines(List(
            Line(1, "irrelevant", None, Some(Org(Number(42)))),
            Line(2, "irrelevant", Some(new Label(fnord)), None))
        )
        model.getLabel(fnord) must be(42)
    }

    @Test
    def exceptionsContainTheirLineNumber(): Unit = {
        // Need to detect that model exceptions are rethrown with their line number by the code generator.
        // Constant reassignment throws an exception, in the model (and this is tested in the model's tests); this is
        // rethrown including line number by the code generator.
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("2: Constant 'FNORD' cannot be redefined; initially defined on line 1")

        generateFromStatements(List(
            ConstantAssignment(new SymbolName(fnord), Number(42)),
            ConstantAssignment(new SymbolName(fnord), Number(12))
        ))
    }

    // The code generator does nothing with macros - they're expanded into other statements; ignore all macro
    // statements. Can't sense anything, but these won't throw, and these tests force the match on the macro AST
    // types to be introduced.
    @Test
    def macroStartIgnored(): Unit = {
        generateFromStatement(MacroStart(new MacroName(fnord), List.empty))
    }

    @Test
    def macroBodyIgnored(): Unit = {
        generateFromStatement(MacroBody("whatever"))
    }

    @Test
    def macroEndIgnored(): Unit = {
        generateFromStatement(MacroEnd())
    }

    @Test
    def macroInvocationIgnored(): Unit = {
        generateFromStatement(MacroInvocation(new MacroName(fnord), List.empty))
    }

    // Precondition for the DB/DD/DW, checked for by the parser: the expressions are non-empty lists.
    @Test
    def dbNumbers(): Unit = {
        val cellWidth = 1
        val dbStatement = DB(List(Number(42), Number(69)))
        val line = Line(1, "", None, Some(dbStatement))
        val model = generateFromLine(line)

        val storages = model.getStoragesForLine(1)
        storages must have size 1
        val storage = storages.head
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(42, 69))
        storage.line must be(line)
        model.getDollar must be(0 + (cellWidth * 2))
    }

    @Test
    def dwNumbers(): Unit = {
        val cellWidth = 2
        val dwStatement = DW(List(Number(42), Number(69)))
        val line = Line(1, "", None, Some(dwStatement))
        val model = generateFromLine(line)

        val storages = model.getStoragesForLine(1)
        storages must have size 1
        val storage = storages.head
        storage.address must be(0)
        storage.cellWidth must be(2)
        storage.data.toList must be(List(42, 69))
        storage.line must be(line)
        model.getDollar must be(0 + (cellWidth * 2))
    }

    @Test
    def ddNumbers(): Unit = {
        val cellWidth = 4
        val ddStatement = DD(List(Number(42), Number(69)))
        val line = Line(1, "", None, Some(ddStatement))
        val model = generateFromLine(line)

        val storages = model.getStoragesForLine(1)
        storages must have size 1
        val storage = storages.head
        storage.address must be(0)
        storage.cellWidth must be(4)
        storage.data.toList must be(List(42, 69))
        storage.line must be(line)
        model.getDollar must be(0 + (cellWidth * 2))
    }

    @Test
    def dbDupNumbers(): Unit = {
        val cellWidth = 1
        val count = 5
        val dbDupStatement = DBDup(Number(count), Number(69))
        val line = Line(1, "", None, Some(dbDupStatement))
        val model = generateFromLine(line)

        val storages = model.getStoragesForLine(1)
        storages must have size 1
        val storage = storages.head
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(69, 69, 69, 69, 69))
        storage.line must be(line)
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

        val storages = model.getStoragesForLine(1)
        storages must have size 1
        val storage = storages.head
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List.empty)
        storage.line must be(line)
        model.getDollar must be(0)
    }

    @Test
    def checkForUnresolvedForwardReferencesAtCreateModelTime(): Unit = {
        // Set up the model with an unresolved forward reference, that'll cause the unresolved forward reference check
        // to throw. (Only if that check is called in createModel)
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Forward references remain unresolved at end of Pass 1: (FNORD: #1)")

        val dbStatement = DB(List(SymbolArg(fnord)))
        val line = Line(1, "", None, Some(dbStatement))

        generateFromLine(line)
    }

    @Test
    def addressOfPass1BlockStoredOnIf1(): Unit = {
        generateFromLines(List(
            Line(1, "", None, Some(Org(Number(42)))),
            Line(2, "", None, Some(If1()))
        ))

        codegen.currentP2Structure.getStartAddress must be(42)
    }

    @Test
    def sizeOfPass1BlockStoredOnElse(): Unit = {
        generateFromLines(List(
            Line(1, "", None, Some(If1())),
            Line(2, "", None, Some(DB(List(Number(1), Number(2), Number(3))))), // 3 bytes in the block
            Line(3, "", None, Some(Else()))
        ))

        codegen.currentP2Structure.getPass1BlockSize must be(3)
    }

    @Test
    def pass2LinesStoredInTheCurrentP2Structure(): Unit = {
        val lines = List(
            Line(1, "", None, Some(If1())), // No lines in pass 1 - but there will be in pass 2.
            Line(2, "", None, Some(Else())), // That will throw on Endif, but I just need to sense storage of pass 2 lines.
            Line(3, "", None, Some(DB(List(Number(6), Number(7), Number(8))))), // updated values in pass 2
            Line(4, "", None, Some(DW(List(Number(9), Number(10)))))
        )
        val model = generateFromLines(lines)

        // The Pass 2 lines are stored in the current P2 structure....
        codegen.currentP2Structure.getPass2Lines must be(List(lines(2), lines(3)))

        // ...and not yet stored in the list for Pass 2
        codegen.p2Structures must be(empty)

        // ...and the pass 2 lines haven't been stored normally.
        val storages = model.getStoragesForLine(3)
        storages must be(empty)
    }

    @Test
    def builtPass2LinesRecordedOnEndif(): Unit = {
        generateFromLines(List(
            Line(1, "", None, Some(Org(Number(42)))),
            Line(2, "", None, Some(If1())),
            Line(3, "", None, Some(DB(List(Number(1), Number(2), Number(3))))),
            Line(4, "", None, Some(DW(List(Number(4), Number(5))))),
            Line(5, "", None, Some(DD(List(Number(0))))),
            Line(6, "", None, Some(Else())),
            Line(7, "", None, Some(DB(List(Number(6), Number(7), Number(8))))),
            Line(8, "", None, Some(DW(List(Number(9), Number(10))))),
            Line(9, "", None, Some(DD(List(Number(11))))),
            Line(10, "", None, Some(Endif()))
        ))

        // Structure is recorded for processing in pass 2....
        val p2stored = codegen.p2Structures.head
        p2stored.getStartAddress must be(42)
        p2stored.getPass1BlockSize must be(11)

        // We have a new structure for building up....
        codegen.currentP2Structure.getPass1BlockSize must be(0)
        codegen.currentP2Structure.getPass2Lines must be(empty)
    }

    @Test
    def returnToPass1AssemblyAfterEndif(): Unit = {
        val model = generateFromLines(List(
            Line(1, "", None, Some(Org(Number(42)))),
            Line(2, "", None, Some(If1())),
            Line(3, "", None, Some(Else())),
            Line(4, "", None, Some(Endif())),
            Line(5, "", None, Some(DD(List(Number(11)))))
        ))

        // Line 5 is stored... since we're back in Assembly (conversion to Storage) mode after Endif
        val storages = model.getStoragesForLine(5)
        storages must have size 1
        val line5Storage = storages.head
        line5Storage.line.number must be(5)
        line5Storage.data must have size 1
        line5Storage.data(0) must be(11)
    }

    @Test
    def elseWithoutIf1(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("3: Else seen without prior If1")
        generateFromLines(List(Line(3, "", None, Some(Else()))))
    }

    @Test
    def endifWithoutIf1(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("7: Endif seen without prior If1")
        generateFromLines(List(Line(7, "", None, Some(Endif()))))
    }

    @Test
    def fullIf1ElseEndifTest(): Unit = {
        val model = generateFromLines(List(
            Line(1, "", None, Some(Org(Number(42)))),
            Line(2, "", Some(new Label(fnord)), Some(DB(List(Number(77))))), // label fnord = 42
            Line(3, "", None, Some(If1())),
            Line(4, "", None, Some(DB(List(Number(1), Number(2), Number(3))))),
            Line(5, "", None, Some(DW(List(Number(4), Number(5))))),
            Line(6, "", None, Some(DD(List(Number(0))))),
            Line(6, "", None, Some(Else())),
            Line(7, "", None, Some(DB(List(Number(6), Number(7), Number(8))))), // updated values in pass 2
            Line(8, "", None, Some(DW(List(Number(9), Number(10))))),
            Line(9, "", None, Some(DD(List(SymbolArg(fnord))))), // should get fixed up in pass 2
            Line(10, "", None, Some(Endif())),
            Line(11, "", None, Some(DB(List(Number(11)))))
        ))

        model.getLabel(fnord) must be(42)

        val line2Storages = model.getStoragesForLine(2)
        line2Storages must have size 1
        val line2Storage = line2Storages.head
        line2Storage.address must be(42)
        line2Storage.data must be(Array(77))
        line2Storage.cellWidth must be(1)

        val line4Storages = model.getStoragesForLine(4)
        line4Storages must have size 1
        val line4Storage = line4Storages.head
        line4Storage.address must be(43)
        line4Storage.data must be(Array(1, 2, 3))
        line4Storage.cellWidth must be(1)

        val line7Storages = model.getStoragesForLine(7)
        line7Storages must have size 1
        val line7Storage = line7Storages.head
        line7Storage.address must be(43)
        line7Storage.data must be(Array(6, 7, 8))
        line7Storage.cellWidth must be(1)

        val line9Storages = model.getStoragesForLine(9)
        line9Storages must have size 1
        val line9Storage = line9Storages.head
        line9Storage.address must be(50)
        line9Storage.data must be(Array(42)) // fnord is resolved
        line9Storage.cellWidth must be(4)

        val line11Storages = model.getStoragesForLine(11)
        line11Storages must have size 1
        val line11Storage = line11Storages.head
        line11Storage.address must be(54)
        line11Storage.data must be(Array(11))
        line11Storage.cellWidth must be(1)
    }

    @Test
    def differentPass1AndPass2BlockSizes(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("4: Differently-sized blocks in Passes 1 and 2: Pass 1=3 byte(s); Pass 2=4 byte(s)")

        generateFromLines(List(
            Line(1, "", None, Some(If1())),
            Line(2, "", None, Some(DB(List(Number(1), Number(2), Number(3))))),
            Line(3, "", None, Some(Else())),
            Line(4, "", None, Some(DW(List(Number(9), Number(10))))),
            Line(5, "", None, Some(Endif()))
        ))
    }

    @Test
    def largestLineNumberCollected(): Unit = {
        generateFromLines(List(
            Line(1, "", None, None),
            Line(2, "", None, None),
            Line(3, "", None, None)
        ))
        codegen.getLastLineNumber must be (3)
    }
}
