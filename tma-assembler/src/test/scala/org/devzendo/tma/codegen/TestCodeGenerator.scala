/*
 * Copyright (C) 2008-2023 Matt Gumbley, DevZendo.org http://devzendo.org
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

import org.devzendo.tma.SourceLocation
import org.devzendo.tma.ast.AST.{Label, MacroArgument, MacroName, SymbolName}
import org.devzendo.tma.ast.{ConstantAssignment, Line, VariableAssignment, _}
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.log4s.Logger
import org.scalatest.DiagrammedAssertions.diagrammedAssertionsHelper
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.{DiagrammedAssertions, MustMatchers}

import scala.collection.mutable.ArrayBuffer

class TestCodeGenerator extends CodeGeneratorFixture with SourcedValuesFixture with AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger
    val dollar = "$"
    val fnord = "FNORD"
    val fnordCasedSymbolName = CasedSymbolName(fnord)
    val fnordSymbolArg = SymbolArg(fnordCasedSymbolName.toString)

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    @Test
    def initialConditions(): Unit = {
        codegen.getLastLineNumber must be (0)
        val model = codegen.createModel(List(Line(SourceLocation("", 1), "", None, Some(End(None)))))
        model.title must be("")
        model.rows must be(25)
        model.columns must be(80)
        model.processor must be(None)
        model.getDollar must be(0)
        model.getVariable(CasedSymbolName(dollar)) must be (0)
        model.hasEndBeenSeen must be (true) // cannot sense it being false initially after model created
        codegen.getLastLineNumber must be(1)
    }

    @Test
    def linesAreAddedToModel(): Unit = {
        val originalLine = Line(SourceLocation("", 1), "; text of line", None, None)
        val model = generateFromLine(originalLine)

        // foreachLineSourcedValues is the only way to get original/transformed Lines out...
        var calls = 0
        model.foreachLineSourcedValues((line: IndexedLine, sourcedValues: List[SourcedValue]) => {
            calls += 1
            if (calls > 1) {
                fail("Should have only called back once")
            }

            line must be(toIndexedLine(originalLine))
            sourcedValues must be(empty)
        })
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
    def processorDefaultEndianNess(): Unit = {
        val model = generateFromStatement(Title("Irrelevant"))
        model.processor must be(None)
        model.endianness must be(Endianness.Big)
    }

    @Test
    def processorTransputer(): Unit = {
        val model = generateFromStatement(Processor("TRANSPUTER"))
        model.processor must be(Some("TRANSPUTER"))
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
            val inmodel = new AssemblyModel(true)
            val localCodeGen = new CodeGenerator(true, inmodel)
            val model = localCodeGen.createModel(List(
                Line(SourceLocation("", 1), "", None, Some(Org(Number(i)))),
                Line(SourceLocation("", 2), "", None, Some(Align(8))),
                Line(SourceLocation("", 3), "", None, Some(End(None)))
            ))
            model.getDollar must be(8)
        }
    }

    @Test
    def statementTransformationExceptionsRethrownAsCodeGenerationExceptions(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("1: Boom!")

        def explode(st: Statement): Statement = {
            throw new StatementTransformationException("Boom!")
        }
        codegen.addStatementTransformer(explode)

        generateFromLine(Line(SourceLocation("", 1), "", None, Some(Ignored())))
    }

    @Test
    def end(): Unit = {
        val model = generateFromLines(List(Line(SourceLocation("", 1), "", None, Some(End(None)))))
        codegen.endCheck()
        // Gets to the end, does not throw!
        model.hasEndBeenSeen must be (true)
    }

    @Test
    def endMissing(): Unit = {
        generateFromLines(List(Line(SourceLocation("", 1), "", None, None)))
        codegen.endCheck()
        codegen.getCodeGenerationExceptions.head.getMessage must be("1: End of input reached with no End statement")
    }

    @Test
    def endWithSubsequentCodeInPass1Disallowed(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("2: No statements allowed after End statement")

        generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(End(None))),
            Line(SourceLocation("", 2), "", None, Some(DB(List(Number(5)))))
        ))
        // no need to call endCheck, code gen will fail before then
    }

    @Test
    def endWithSubsequentCodeInPass2FixupsAllowed(): Unit = {
        generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(If1())),
            Line(SourceLocation("", 2), "", None, Some(DD(List(Number(0))))),
            Line(SourceLocation("", 3), "", None, Some(Else())),
            Line(SourceLocation("", 4), "", None, Some(DD(List(SymbolArg(fnord))))), // should get fixed up in pass 2
            Line(SourceLocation("", 5), "", None, Some(Endif())),
            Line(SourceLocation("", 6), "", None, Some(ConstantAssignment(new SymbolName(fnord), Number(11)))),
            Line(SourceLocation("", 7), "", None, Some(End(None)))
        ))

        codegen.endCheck()
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
        model.getVariable(CasedSymbolName(dollar)) must be(42)
    }

    @Test
    def orgDefinedVariable(): Unit = {
        val model = generateFromStatements(List(
            ConstantAssignment(new SymbolName(fnord), Number(42)),
            Org(SymbolArg(fnord))
        ))
        model.getDollar must be(42)
        model.getVariable(CasedSymbolName(dollar)) must be(42)
    }

    @Test
    def constantAssignment(): Unit = {
        val model = generateFromStatement(ConstantAssignment(new SymbolName(fnord), Number(42)))
        model.getConstant(CasedSymbolName(fnord)) must be(42)
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
        // Can't test the detail of this in code gen - the assembly model is where the mechanism is, see
        // TestAssemblyModel::constantSetWithUnresolvedSymbolsGetsResolvedAsTheSymbolsAreDefined
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("0: Symbol forward references remain unresolved at end of Pass 1: (UNDEF: #1)")
        generateFromStatement(ConstantAssignment(new SymbolName(fnord),SymbolArg("undef")))
    }

    @Test
    def constantAssignmentToUndefinedSymbolIsFixedUpOnDefinitionOfConstant(): Unit = {
        val model = generateFromStatements(List(
            ConstantAssignment(new SymbolName(fnord),SymbolArg("undef")),
            ConstantAssignment(new SymbolName("undef"),Number(42))) // causes fixup
        )

        model.resolutionCount(CasedSymbolName("undef")) must be (1)

        model.getConstant(CasedSymbolName("fnord")) must be(42)
    }

    @Test
    def constantAssignmentToUndefinedSymbolIsFixedUpOnDefinitionOfVariable(): Unit = {
        val model = generateFromStatements(List(
            ConstantAssignment(new SymbolName(fnord),SymbolArg("undef")),
            VariableAssignment(new SymbolName("undef"),Number(42))) // causes fixup
        )

        model.resolutionCount(CasedSymbolName("undef")) must be (1)

        model.getConstant(CasedSymbolName("fnord")) must be(42)
    }

    @Test
    def constantAssignmentToUndefinedSymbolIsFixedUpOnDefinitionOfLabel(): Unit = {
        val model = generateFromLines(List(
            Line(SourceLocation("", 1), "FNORD EQU undef", None, Some(ConstantAssignment(new SymbolName(fnord),SymbolArg("undef")))),
            Line(SourceLocation("", 2), "ORG 42", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 3), "undef:", Some("undef"), None)
        ))

        model.resolutionCount(CasedSymbolName("undef")) must be (1)

        model.getConstant(CasedSymbolName("fnord")) must be(42)
    }

    @Test
    def constantAssignmentToUndefinedSymbolIsFixedUpOnDefinitionOfVariableAndTracksChanges(): Unit = {
        // if this were in convergence, constant redefinition would be OK.
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("3: Constant 'FNORD' cannot override existing constant; defined on line 1")

        generateFromStatements(List(
            ConstantAssignment(new SymbolName(fnord), SymbolArg("undef")),
            VariableAssignment(new SymbolName("undef"), Number(42)), // causes fixup
            VariableAssignment(new SymbolName("undef"), Number(69))) // boom
        )
    }

    @Test
    def variableAssignment(): Unit = {
        val model = generateFromStatement(VariableAssignment(new SymbolName(fnord), Number(42)))
        model.getVariable(CasedSymbolName(fnord)) must be(42)
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
        // Can't test the detail of this in code gen - the assembly model is where the mechanism is, see
        // TestAssemblyModel::variableSetWithUnresolvedSymbolsGetsResolvedAsTheSymbolsAreDefined
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("0: Symbol forward references remain unresolved at end of Pass 1: (UNDEF: #1)")
        generateFromStatement(VariableAssignment(new SymbolName(fnord),SymbolArg("undef")))
    }

    @Test
    def variableAssignmentToUndefinedSymbolIsFixedUpOnDefinitionOfConstant(): Unit = {
        val model = generateFromStatements(List(
            VariableAssignment(new SymbolName(fnord),SymbolArg("undef")),
            ConstantAssignment(new SymbolName("undef"),Number(42))) // causes fixup
        )

        model.resolutionCount(CasedSymbolName("undef")) must be (1)

        model.getVariable(CasedSymbolName("fnord")) must be(42)
    }

    @Test
    def variableAssignmentToUndefinedSymbolIsFixedUpOnDefinitionOfVariable(): Unit = {
        val model = generateFromStatements(List(
            VariableAssignment(new SymbolName(fnord),SymbolArg("undef")),
            VariableAssignment(new SymbolName("undef"),Number(42))) // causes fixup
        )

        model.resolutionCount(CasedSymbolName("undef")) must be (1)

        model.getVariable(CasedSymbolName("fnord")) must be(42)
    }

    @Test
    def variableAssignmentToUndefinedSymbolIsFixedUpOnDefinitionOfLabel(): Unit = {
        val model = generateFromLines(List(
            Line(SourceLocation("", 1), "FNORD = undef", None, Some(VariableAssignment(new SymbolName(fnord),SymbolArg("undef")))),
            Line(SourceLocation("", 2), "ORG 42", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 3), "undef:", Some("undef"), None)
        ))

        model.resolutionCount(CasedSymbolName("undef")) must be (1)

        model.getVariable(CasedSymbolName("fnord")) must be(42)
    }

    // NOTE: Constants and Labels can be used in expressions when they're not yet defined, and when they are, the
    // references will be updated, and will track changes to the labels and constants (typically such changes are only
    // allowed during convergence: labels and constants are otherwise only assigned once).
    // Variables can be used in expressions when they're not yet defined, and when they are, the references will be
    // updated, but they will not track changes - as variables can change throughout the assembly.
    @Test
    def variableAssignmentToUndefinedSymbolIsFixedUpOnDefinitionAndShouldNotTrackChanges(): Unit = {
        val model = generateFromStatements(List(
            VariableAssignment(new SymbolName(fnord),SymbolArg("undef")),
            VariableAssignment(new SymbolName("undef"),Number(42)), // causes fixup of fnord
            VariableAssignment(new SymbolName("undef"),Number(69))) // does not cause change to fnord
        )

        model.resolutionCount(CasedSymbolName("undef")) must be (1)

        model.getVariable(CasedSymbolName("fnord")) must be(42)
        model.getVariable(CasedSymbolName("undef")) must be(69)
    }

    @Test
    def variableReassignmentAllowed(): Unit = {
        val model = generateFromStatements(List(
            VariableAssignment(new SymbolName(fnord), Number(42)),
            VariableAssignment(new SymbolName(fnord), Number(12))
        ))
        model.getVariable(CasedSymbolName(fnord)) must be(12)
    }

    @Test
    def labelAssignmentFromDollar(): Unit = {
        val model = generateFromLines(List(
            Line(SourceLocation("", 1), "irrelevant", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 2), "irrelevant", Some(new Label(fnord)), None))
        )
        model.getLabel(CasedSymbolName(fnord)) must be(42)
    }

    @Test
    def exceptionsContainTheirLineNumber(): Unit = {
        // Need to detect that model exceptions are rethrown with their line number by the code generator.
        // Constant reassignment throws an exception, in the model (and this is tested in the model's tests); this is
        // rethrown including line number by the code generator.
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("2: Constant 'FNORD' cannot override existing constant; defined on line 1")

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

    // Test helper that assumes your test lines start at line 1
    private def toIndexedLine(line: Line): IndexedLine = {
        IndexedLine(line.location.lineNumber - 1, line.location, line.text, line.label, line.stmt)
    }
    private def toLine(indexedLine: IndexedLine): Line = {
        Line(indexedLine.location, indexedLine.text, indexedLine.label, indexedLine.stmt)
    }

    // Precondition for the DB/DD/DW, checked for by the parser: the expressions are non-empty lists.
    @Test
    def dbNumbers(): Unit = {
        val cellWidth = 1
        val dbStatement = DB(List(Number(42), Number(69)))
        val line = Line(SourceLocation("", 1), "", None, Some(dbStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(42, 69))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 2))
    }

    @Test
    def dbCharacters(): Unit = {
        val cellWidth = 1
        val dbStatement = DB(List(Characters("Ab0")))
        val line = Line(SourceLocation("", 1), "", None, Some(dbStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(65, 98, 48))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 3))
    }

    @Test
    def dbMixedNumbersAndCharacters(): Unit = {
        val cellWidth = 1
        val dbStatement = DB(List(Number(5), Characters("abc"), Number(7)))
        val line = Line(SourceLocation("", 1), "", None, Some(dbStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(5, 97, 98, 99, 7))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 5))
    }

    @Test
    def dwNumbers(): Unit = {
        val cellWidth = 2
        val dwStatement = DW(List(Number(42), Number(69)))
        val line = Line(SourceLocation("", 1), "", None, Some(dwStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(2)
        storage.data.toList must be(List(42, 69))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 2))
    }

    @Test
    def dwCharacters(): Unit = {
        val cellWidth = 2
        val dwStatement = DW(List(Characters("Ab0")))
        val line = Line(SourceLocation("", 1), "", None, Some(dwStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(2)
        storage.data.toList must be(List(65, 98, 48))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 3))
    }

    @Test
    def dwMixedNumbersAndCharacters(): Unit = {
        val cellWidth = 2
        val dwStatement = DW(List(Number(5), Characters("abc"), Number(7)))
        val line = Line(SourceLocation("", 1), "", None, Some(dwStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(2)
        storage.data.toList must be(List(5, 97, 98, 99, 7))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 5))
    }

    @Test
    def ddNumbers(): Unit = {
        val cellWidth = 4
        val ddStatement = DD(List(Number(42), Number(69)))
        val line = Line(SourceLocation("", 1), "", None, Some(ddStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(4)
        storage.data.toList must be(List(42, 69))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 2))
    }

    @Test
    def ddCharacters(): Unit = {
        val cellWidth = 4
        val ddStatement = DD(List(Characters("Ab0")))
        val line = Line(SourceLocation("", 1), "", None, Some(ddStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(4)
        storage.data.toList must be(List(65, 98, 48))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 3))
    }

    @Test
    def ddMixedNumbersAndCharacters(): Unit = {
        val cellWidth = 4
        val ddStatement = DD(List(Number(5), Characters("abc"), Number(7)))
        val line = Line(SourceLocation("", 1), "", None, Some(ddStatement))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(4)
        storage.data.toList must be(List(5, 97, 98, 99, 7))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 5))
    }

    @Test
    def checkForUnresolvedStorageForwardReferencesAtCreateModelTime(): Unit = {
        // Set up the model with an unresolved storage forward reference, that'll cause the unresolved forward reference
        // check to throw. (Only if that check is called in createModel)
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("Storage forward references remain unresolved at end of Pass 1: (FNORD: #1)")

        val dbStatement = DB(List(SymbolArg(fnord)))
        val line = Line(SourceLocation("", 1), "", None, Some(dbStatement))

        generateFromLine(line)
    }

    @Test
    def checkForUnresolvedSymbolForwardReferencesAtCreateModelTime(): Unit = {
        // Set up the model with an unresolved symbol forward reference, that'll cause the unresolved forward reference
        // check to throw. (Only if that check is called in createModel)
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("Symbol forward references remain unresolved at end of Pass 1: (UNDEF: #1)")

        val caStatement = ConstantAssignment(new SymbolName(fnord), SymbolArg("undef"))
        val line = Line(SourceLocation("", 1), "", None, Some(caStatement))

        generateFromLine(line)
    }

    @Test
    def checkForUnresolvedSymbolForwardReferencesInInstructionAtCreateModelTime(): Unit = {
        // Set up the model with an unresolved symbol forward reference, that'll cause the unresolved forward reference
        // check to throw. (Only if that check is called in createModel)
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("0: Symbol forward references remain unresolved at end of Pass 1 (still converging): Set(FNORD)")

        generateFromLines(List(
            Line(SourceLocation("", 1), ".TRANSPUTER", None, Some(Processor("TRANSPUTER"))),
            Line(SourceLocation("", 2), "CALL FNORD", None, Some(DirectInstruction("CALL", 144, SymbolArg("FNORD")))),
            Line(SourceLocation("", 3), "END", None, Some(End(None)))
        ))
    }

    @Test
    def addressOfPass1BlockStoredOnIf1(): Unit = {
        generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 2), "", None, Some(If1()))
        ))

        codegen.currentP2Structure.getStartAddress must be(42)
    }

    @Test
    def sizeOfPass1BlockStoredOnElse(): Unit = {
        generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(If1())),
            Line(SourceLocation("", 2), "", None, Some(DB(List(Number(1), Number(2), Number(3))))), // 3 bytes in the block
            Line(SourceLocation("", 3), "", None, Some(Else()))
        ))

        codegen.currentP2Structure.getPass1BlockSize must be(3)
    }

    @Test
    def pass2LinesStoredInTheCurrentP2Structure(): Unit = {
        val lines = List(
            Line(SourceLocation("", 1), "", None, Some(If1())), // No lines in pass 1 - but there will be in pass 2.
            Line(SourceLocation("", 2), "", None, Some(Else())), // That will throw on Endif, but I just need to sense storage of pass 2 lines.
            Line(SourceLocation("", 3), "", None, Some(DB(List(Number(6), Number(7), Number(8))))), // updated values in pass 2
            Line(SourceLocation("", 4), "", None, Some(DW(List(Number(9), Number(10)))))
        )
        val model = generateFromLines(lines)

        // The Pass 2 lines are stored in the current P2 structure....
        codegen.currentP2Structure.getPass2Lines must be(List(toIndexedLine(lines(2)), (toIndexedLine(lines(3)))))

        // ...and not yet stored in the list for Pass 2
        codegen.p2Structures must be(empty)

        // ...and the pass 2 lines haven't been stored normally.
        val storages = model.getSourcedValuesForLineIndex(2)
        storages must be(empty)
    }

    @Test
    def builtPass2LinesRecordedOnEndif(): Unit = {
        generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 2), "", None, Some(If1())),
            Line(SourceLocation("", 3), "", None, Some(DB(List(Number(1), Number(2), Number(3))))),
            Line(SourceLocation("", 4), "", None, Some(DW(List(Number(4), Number(5))))),
            Line(SourceLocation("", 5), "", None, Some(DD(List(Number(0))))),
            Line(SourceLocation("", 6), "", None, Some(Else())),
            Line(SourceLocation("", 7), "", None, Some(DB(List(Number(6), Number(7), Number(8))))),
            Line(SourceLocation("", 8), "", None, Some(DW(List(Number(9), Number(10))))),
            Line(SourceLocation("", 9), "", None, Some(DD(List(Number(11))))),
            Line(SourceLocation("", 10), "", None, Some(Endif()))
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
            Line(SourceLocation("", 1), "", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 2), "", None, Some(If1())),
            Line(SourceLocation("", 3), "", None, Some(Else())),
            Line(SourceLocation("", 4), "", None, Some(Endif())),
            Line(SourceLocation("", 5), "", None, Some(DD(List(Number(11)))))
        ))

        // Line 5 is stored... since we're back in Assembly (conversion to Storage) mode after Endif
        val storages = model.getSourcedValuesForLineIndex(4)
        storages must have size 1
        val line5Storage = singleStorage(storages)
        line5Storage.indexedLine.location.lineNumber must be(5)
        line5Storage.data must have size 1
        line5Storage.data(0) must be(11)
    }

    @Test
    def elseWithoutIf1(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("3: Else seen without prior If1")
        generateFromLines(List(Line(SourceLocation("", 3), "", None, Some(Else()))))
    }

    @Test
    def endifWithoutIf1(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("7: Endif seen without prior If1")
        generateFromLines(List(Line(SourceLocation("", 7), "", None, Some(Endif()))))
    }

    @Test
    def fullIf1ElseEndifTest(): Unit = {
        val model = generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 2), "", Some(new Label(fnord)), Some(DB(List(Number(77))))), // label fnord = 42
            Line(SourceLocation("", 3), "", None, Some(If1())),
            Line(SourceLocation("", 4), "", None, Some(DB(List(Number(1), Number(2), Number(3))))),
            Line(SourceLocation("", 5), "", None, Some(DW(List(Number(4), Number(5))))),
            Line(SourceLocation("", 6), "", None, Some(DD(List(Number(0))))),
            Line(SourceLocation("", 7), "", None, Some(Else())),
            Line(SourceLocation("", 8), "", None, Some(DB(List(Number(6), Number(7), Number(8))))), // updated values in pass 2
            Line(SourceLocation("", 9), "", None, Some(DW(List(Number(9), Number(10))))),
            Line(SourceLocation("", 10), "", None, Some(DD(List(SymbolArg(fnord))))), // should get fixed up in pass 2
            Line(SourceLocation("", 11), "", None, Some(Endif())),
            Line(SourceLocation("", 12), "", None, Some(DB(List(Number(11)))))
        ))

        model.getLabel(CasedSymbolName(fnord)) must be(42)

        val line2SourcedValues = model.getSourcedValuesForLineIndex(1)
        line2SourcedValues must have size 2
        val line2Storage = singleStorage(line2SourcedValues)
        line2Storage.address must be(42)
        line2Storage.data must be(Array(77))
        line2Storage.cellWidth must be(1)

        val line2AssignmentValue = singleAssignmentValue(line2SourcedValues)
        line2AssignmentValue.data must be(42)

        val line4Storages = model.getSourcedValuesForLineIndex(3)
        line4Storages must have size 1
        val line4Storage = singleStorage(line4Storages)
        line4Storage.address must be(43)
        line4Storage.data must be(Array(1, 2, 3))
        line4Storage.cellWidth must be(1)

        val line8Storages = model.getSourcedValuesForLineIndex(7)
        line8Storages must have size 1
        val line8Storage = singleStorage(line8Storages)
        line8Storage.address must be(43)
        line8Storage.data must be(Array(6, 7, 8))
        line8Storage.cellWidth must be(1)

        val line10Storages = model.getSourcedValuesForLineIndex(9)
        line10Storages must have size 1
        val line10Storage = singleStorage(line10Storages)
        line10Storage.address must be(50)
        line10Storage.data must be(Array(42)) // fnord is resolved
        line10Storage.cellWidth must be(4)

        val line12Storages = model.getSourcedValuesForLineIndex(11)
        line12Storages must have size 1
        val line12Storage = singleStorage(line12Storages)
        line12Storage.address must be(54)
        line12Storage.data must be(Array(11))
        line12Storage.cellWidth must be(1)
    }

    @Test
    def differentPass1AndPass2BlockSizes(): Unit = {
        thrown.expect(classOf[CodeGenerationException])
        thrown.expectMessage("4: Differently-sized blocks in Passes 1 and 2: Pass 1=3 byte(s); Pass 2=4 byte(s)")

        generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(If1())),
            Line(SourceLocation("", 2), "", None, Some(DB(List(Number(1), Number(2), Number(3))))),
            Line(SourceLocation("", 3), "", None, Some(Else())),
            Line(SourceLocation("", 4), "", None, Some(DW(List(Number(9), Number(10))))),
            Line(SourceLocation("", 5), "", None, Some(Endif()))
        ))
    }

    @Test
    def largestLineNumberCollected(): Unit = {
        generateFromLines(List(
            Line(SourceLocation("", 1), "", None, None),
            Line(SourceLocation("", 2), "", None, None),
            Line(SourceLocation("", 3), "", None, None)
        ))
        codegen.getLastLineNumber must be (3)
    }

    @Test
    def labelAndStorageCauseTwoSourcedValues(): Unit = {
        val model = generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 2), "", Some(new Label(fnord)), Some(DB(List(Number(77)))))
        ))

        val sourcedValues = model.getSourcedValuesForLineIndex(1)
        sourcedValues must have size 2

        val storage = singleStorage(sourcedValues)
        storage.address must be(42)
        storage.data must be(Array(77))
        storage.cellWidth must be(1)

        val assignmentValue = singleAssignmentValue(sourcedValues)
        assignmentValue.symbolType must be(SymbolType.Label)
        assignmentValue.data must be(42)
    }

    @Test
    def labelAndAssignmentCauseTwoSourcedValues(): Unit = {
        val model = generateFromLines(List(
            Line(SourceLocation("", 1), "", None, Some(Org(Number(42)))),
            Line(SourceLocation("", 2), "", Some(new Label(fnord)), Some(VariableAssignment(new SymbolName("foo"), Number(77))))
        ))

        val sourcedValues = model.getSourcedValuesForLineIndex(1)
        sourcedValues must have size 2

        val assignmentValues = sourcedValues.map(_.asInstanceOf[AssignmentValue])

        val label = assignmentValues.filter(_.symbolType == SymbolType.Label).head
        label.data must be(42)

        val assignment = assignmentValues.filter(_.symbolType != SymbolType.Label).head
        assignment.data must be(77)
    }

    @Test
    def foreachLineSourcedValuesGivesAllOriginalLinesAndMacroExpandedLinesAndSourcedValues(): Unit = {
        val line5Exprs = List(SymbolArg("_CODE"), SymbolArg("_LINK"))
        val lines = List(
            Line(SourceLocation("", 11), "EQU\tCELLL\t10", None, Some(ConstantAssignment(new SymbolName("CELLL"), Number(10)))),
            Line(SourceLocation("", 12), "_NAME\t=\t3", None, Some(VariableAssignment(new SymbolName("_NAME"), Number(3)))),
            Line(SourceLocation("", 13), "$CODE\t3,'?RX',QRX", None, Some(MacroInvocation(new MacroName("$CODE"), List(new MacroArgument("3"), new MacroArgument("'?RX'"), new MacroArgument("QRX"))))),
            Line(SourceLocation("", 13), "_CODE\t= $", None, Some(VariableAssignment(new SymbolName("_CODE"), SymbolArg("$")))),
            Line(SourceLocation("", 13), "ORG\t_NAME", None, Some(Org(SymbolArg("_NAME")))),
            Line(SourceLocation("", 13), "DD\t _CODE,_LINK", None, Some(DD(line5Exprs))),
            Line(SourceLocation("", 13), "_LINK\t= $", None, Some(VariableAssignment(new SymbolName("_LINK"), SymbolArg("$")))),
            Line(SourceLocation("", 13), "DB\t3,5", None, Some(DB(List(Number(3), Number(5))))),
            Line(SourceLocation("", 13), "ORG\t_CODE", None, Some(Org(SymbolArg("_CODE"))))
        )
        val expectedDataValues: List[List[Int]] = List(
            List[Int](10),
            List[Int](3),
            List[Int](),
            List[Int](0),
            List[Int](3),
            List[Int](0, 11),
            List[Int](11),
            List[Int](3, 5),
            List[Int](0)
        )
        val model = generateFromLines(lines)

        var index = 0
        model.foreachLineSourcedValues((indexedLine: IndexedLine, sourcedValues: List[SourcedValue]) => {
            logger.info(s"line $indexedLine")
            for (st <- sourcedValues) {
                logger.info(s"   sourcedValue $st")
            }
            DiagrammedAssertions.assert(toLine(indexedLine) == lines(index))

            // just check the data...
            val expectedDataList = expectedDataValues(index)
            if (expectedDataList.isEmpty) {
                sourcedValues must be(empty)
            } else {
                val sourcedDataList = sourcedValues.head match { // there's only one SourcedValue generated by the above test data
                    case storage: Storage => storage.data.toList
                    case assignmentValue: AssignmentValue => List[Int](assignmentValue.data)
                }
                DiagrammedAssertions.assert(sourcedDataList == expectedDataList, "Line Index " + (index + 1))
            }

            index += 1
        })
    }

    @Test
    def indirectInstructionDefined(): Unit = {
        val cellWidth = 1
        val inst = IndirectInstruction("FPUCLRERR", List(0x29, 0x4c, 0x2a, 0xfb))
        val line = Line(SourceLocation("", 1), "FPUCLRERR", None, Some(inst))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(0x29, 0x4c, 0x2a, 0xfb))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 4))
    }

    // Straightforward cases of direct/indirect instructions that can be immediately built into storage...

    @Test
    def directInstructionDefined(): Unit = {
        val cellWidth = 1
        val inst = DirectInstruction("LDC", 0x40, Number(0x0a))
        val line = Line(SourceLocation("", 1), "LDC 0xa", None, Some(inst))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(0x4a))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 1))
    }

    @Test
    def directInstructionDefinedWithPrefixes(): Unit = {
        val cellWidth = 1
        val inst = DirectInstruction("LDC", 0x40, Number(0x1234abcd))
        val line = Line(SourceLocation("", 1), "LDC 0x1234abcd", None, Some(inst))
        val model = generateFromLine(line)

        val storages = model.getSourcedValuesForLineIndex(0)
        storages must have size 1
        val storage = singleStorage(storages)
        storage.address must be(0)
        storage.cellWidth must be(1)
        storage.data.toList must be(List(0x21, 0x22, 0x23, 0x24, 0x2a, 0x2b, 0x2c, 0x4d))
        storage.indexedLine must be(toIndexedLine(line))
        model.getDollar must be(0 + (cellWidth * 8))
    }

    // Test the various cases that need to be checked by lineContainsDirectInstructionWithUndefinedSymbols.
    // One case here will trigger converge mode.

    @Test
    def noStatement(): Unit = {
        val line = IndexedLine(0, SourceLocation("", 1), "FOO:", Some("FOO"), None)
        lcdiwus(line) must be('empty)
    }

    @Test
    def notEitherInstruction(): Unit = {
        val line = IndexedLine(0, SourceLocation("", 1), "DB 5", None, Some(DB(List(Number(5)))))
        lcdiwus(line) must be('empty)
    }

    @Test
    def indirectInstruction(): Unit = {
        val line = IndexedLine(0, SourceLocation("", 1), "RESETCH", None, Some(IndirectInstruction("RESETCH", List(0x21, 0xf2))))
        lcdiwus(line) must be('empty)
    }

    @Test
    def directInstructionWithEvaluatableExpression(): Unit = {
        val line = IndexedLine(0, SourceLocation("", 1), "LDC 5", None, Some(DirectInstruction("LDC", 0x40, Number(5))))
        lcdiwus(line) must be('empty)
    }

    @Test
    def directEncodedInstruction(): Unit = {
        // Since the DirectInstructionOffsetEncoder's job is to convert DirectInstructions with undefined symbols
        // into DirectEncodedInstructions, which are not generated by the parser, it should never see one.. but it
        // shouldn't barf or return true for one.
        val line = IndexedLine(0, SourceLocation("", 1), "LDC 5", None, Some(DirectEncodedInstruction("LDC", List(0x45))))
        lcdiwus(line) must be('empty)
    }

    @Test
    def directInstructionWithUndefinedExpression(): Unit = {
        val line = IndexedLine(0, SourceLocation("", 1), "LDC FOO + BAR", None, Some(DirectInstruction("LDC", 0x40, Binary(Add(), SymbolArg("FOO"), SymbolArg("BAR")))))
        lcdiwus(line) must be(Set(CasedSymbolName("FOO"), CasedSymbolName("BAR")))
    }

    @Test
    def directInstructionWithDefinedExpression(): Unit = {
        val constline = IndexedLine(0, SourceLocation("", 1), "FOO EQU 10", None, Some(ConstantAssignment("FOO", Number(10))))
        model.setConstant(CasedSymbolName("FOO"), 10, constline)
        val line = IndexedLine(1, SourceLocation("", 2), "LDC FOO", None, Some(DirectInstruction("LDC", 0x40, SymbolArg("FOO"))))
        lcdiwus(line) must be('empty)
    }

    private def lcdiwus(indexedLine: IndexedLine) = {
        codegen.lineContainsDirectInstructionWithUndefinedSymbols(indexedLine)
    }

    @Test
    def statementTransformersAreInvokedInOrder(): Unit = {
        val titles = ArrayBuffer[String]()

        def changeTitle(st: Statement): Statement = {
            st match {
                case Title(text) =>
                    titles += text
                    logger.debug("Changing title to 'changed'")
                    Title("changed")
                case _ =>
                    st
            }
        }
        codegen.addStatementTransformer(changeTitle)

        def uppercaseTitle(st: Statement): Statement = {
            st match {
                case Title(text) =>
                    titles += text
                    logger.debug("Changing title to 'UPPER'")
                    Title("UPPER")
                case _ =>
                    st
            }
        }
        codegen.addStatementTransformer(uppercaseTitle)

        generateFromStatement(Title("custom title")).title must be("UPPER")

        titles.toList must be(List("custom title", "changed"))
    }

    @Test
    def transformedStatementsAreReplacedInTheLinesForConvergence(): Unit = {
        def changeTitle(st: Statement): Statement = {
            st match {
                case Title(text) =>
                    Title("changed")
                case _ =>
                    st
            }
        }
        codegen.addStatementTransformer(changeTitle)

        generateFromStatement(Title("original title")) // should mutate stored inputLines

        val line = codegen.inputLines(0)
        val stmt = line.stmt
        stmt match {
            case Some(Title(title)) => {
                title match {
                    case "original title" => fail("Title was not changed in Statement in Line")
                    case "changed" => logger.info("Correctly changed statement in line: " + line)
                }
            }
            case _ => fail("Unexpected statement " + stmt)
        }
    }
}
