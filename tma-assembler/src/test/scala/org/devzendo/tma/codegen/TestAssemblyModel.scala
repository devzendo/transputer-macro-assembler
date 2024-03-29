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
import org.devzendo.tma.ast.AST.SymbolName
import org.devzendo.tma.ast._
import org.junit.rules.ExpectedException
import org.junit.{Ignore, Rule, Test}
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.mutable

class TestAssemblyModel extends AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger
    val dollar = CasedSymbolName("$")
    val foo = CasedSymbolName("foo")
    val fnord = CasedSymbolName("FNORD")
    val fnordSymbolArg = SymbolArg(fnord.toString)

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val model = new AssemblyModel(true)


    @Test
    def initialConditions(): Unit = {
        model.hasEndBeenSeen must be (false)
        model.lowestStorageAddress must be(0)
        model.highestStorageAddress must be(0)
        model.getConvergeMode must be(false)
    }

    // Variables -------------------------------------------------------------------------------------------------------

    private def genDummyLine(lineNumber: Int) = IndexedLine(lineNumber - 1, SourceLocation("", lineNumber), "", None, None)

    @Test
    def unknownVariableRetrieval(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Variable '" + fnord + "' has not been defined")
        model.getVariable(fnord)
    }

    @Test
    def dollarIsAlwaysPresentAndInitialised(): Unit = {
        model.getVariable(dollar) must be(0)
        model.getDollar must be(0)
    }

    @Test
    def dollarCanBeSet(): Unit = {
        model.setDollar(50, genDummyLine(0))
        model.getDollar must be(50)
        model.setVariable(dollar, 23, genDummyLine(0))
        model.getDollar must be(23)
        model.getVariable(dollar) must be(23)
    }

    @Test
    def setDollarCreatesAnAssignmentValue(): Unit = {
        model.setDollar(42, genDummyLine(4))
        checkAssignmentOfValue(false)
    }

    @Test
    def setDollarSilentlyDoesNotCreateAnAssignmentValue(): Unit = {
        model.setDollarSilently(42)

        numberOfSourcedValues must be(0)
    }

    @Test
    def incrementDollarSilentlyDoesNotCreateAnAssignmentValue(): Unit = {
        model.incrementDollar(42)

        numberOfSourcedValues must be(0)
    }

    private def numberOfSourcedValues = {
        var count = 0
        model.foreachSourcedValue((_: Int, _: List[SourcedValue]) => {
            count += 1
        })
        count
    }

    @Test
    def variableCanBeSet(): Unit = {
        model.setVariable(fnord, 69, genDummyLine(0))
        model.getVariable(fnord) must be(69)
        model.variable(fnord) must be(Some(69))
        model.constant(fnord) must be(None) // it's a variable, not a constant, nor a label
        model.label(fnord) must be(None)
        model.getLabelsAndConstants must be(empty)
    }

    @Test
    def variableNamesCaseInsensitivity(): Unit = {
        CasedSymbolName.setCaseSensitivity(false)

        model.setVariable(CasedSymbolName("fnord"), 69, genDummyLine(1))
        model.getVariable(CasedSymbolName("FNORD")) must be(69)
        model.getVariable(CasedSymbolName("fnord")) must be(69)
        model.getVariable(CasedSymbolName("FnORd")) must be(69)
    }

    @Test
    def variableNamesCaseSensitivity(): Unit = {
        CasedSymbolName.setCaseSensitivity(true)

        try {
            model.setVariable(CasedSymbolName("fnord"), 69, genDummyLine(1))
            model.setVariable(CasedSymbolName("FnORd"), 42, genDummyLine(2))
            model.getVariable(CasedSymbolName("fnord")) must be(69)
            model.getVariable(CasedSymbolName("FnORd")) must be(42)
        } finally {
            CasedSymbolName.setCaseSensitivity(false)
        }
    }

    @Test
    def variableCanBeRedefined(): Unit = {
        model.setVariable(fnord, 32, genDummyLine(0))
        model.setVariable(fnord, 69, genDummyLine(0))
        model.getVariable(fnord) must be(69)
    }

    @Test
    def variableCannotBeDefinedOverExistingConstant(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Variable 'FNORD' cannot override existing constant; initially defined on line 1")

        model.setConstant(fnord, 69, genDummyLine(1))
        model.setVariable(fnord, 17, genDummyLine(2))
    }

    @Test
    def variableCannotBeDefinedOverExistingLabel(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Variable 'FNORD' cannot override existing label; initially defined on line 1")

        model.setLabel(fnord, 69, genDummyLine(1))
        model.setVariable(fnord, 17, genDummyLine(2))
    }

    @Test
    def variableCannotBeDefinedOverExistingConstantEvenInConvergeMode(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Variable 'FNORD' cannot override existing constant; initially defined on line 1")

        model.setConvergeMode(true)

        model.setConstant(fnord, 69, genDummyLine(1))
        model.setVariable(fnord, 17, genDummyLine(2))
    }

    @Test
    def variableCannotBeDefinedOverExistingLabelEvenInConvergeMode(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Variable 'FNORD' cannot override existing label; initially defined on line 1")

        model.setConvergeMode(true)

        model.setLabel(fnord, 69, genDummyLine(1))
        model.setVariable(fnord, 17, genDummyLine(2))
    }

    @Test
    def variableSetWithUnresolvedSymbolsGetsResolvedAsTheSymbolsAreDefined(): Unit = {
        logger.info("*** Start of test")
        // A = B + (C * 2)
        // where B and C are undefined
        val unresolvableExpr = Binary(Add(), SymbolArg("B"), Binary(Mult(), SymbolArg("C"), Number(2)))
        val line = IndexedLine(0, SourceLocation("", 1), "A = B + (C * 2)", None, Some(VariableAssignment(new SymbolName("A"), unresolvableExpr)))
        // When codegen processes the =, it'll do...
        model.recordSymbolForwardReferences(Set(CasedSymbolName("B"), CasedSymbolName("C")), CasedSymbolName("A"), unresolvableExpr, line, SymbolType.Variable)
        // which will record A as an unresolvable symbol keyed by (i.e. resolvable when) B and C are defined.
        // A is a variable, so as it's undefined, it'll get fixed up when B and C are defined, but not if they
        // change subsequently.

        model.resolutionCount(CasedSymbolName("B")) must be (0)
        model.resolutionCount(CasedSymbolName("C")) must be (0)

        logger.info("*** Checking initial state")
        if (true) { // just for fresh scope
            val symbolsB = model.unresolvedSymbolForwardReferences(CasedSymbolName("B"))
            symbolsB must have size 1
            symbolsB.head.casedSymbolName.toString must be("A") // B is needed by A
            symbolsB.head.indexedLine must be(line)
            symbolsB.head.symbolType must be(SymbolType.Variable)
            symbolsB.head.expr must be(unresolvableExpr)

            val symbolsC = model.unresolvedSymbolForwardReferences(CasedSymbolName("C"))
            symbolsC must have size 1
            symbolsC.head.casedSymbolName.toString must be("A") // C is needed by A
            // won't bother checking the rest of the detail that was checked above..

            model.getLabelsAndConstants must be(empty)
        }

        logger.info("*** Defining B")
        // Now let's define B. Then only C will be unresolvable. B should exist, A and C should not.
        val bLine = IndexedLine(1, SourceLocation("", 2), "B EQU 3", None, Some(ConstantAssignment(new SymbolName("B"), Number(3))))
        model.setConstant(CasedSymbolName("B"), 3, bLine)
        logger.info("*** Checking state after defining B")
        if (true) { // just for fresh scope
            model.resolutionCount(CasedSymbolName("B")) must be (1)
            val symbolsB = model.unresolvedSymbolForwardReferences(CasedSymbolName("B"))
            symbolsB must have size 1 // B still needed by A

            model.resolutionCount(CasedSymbolName("C")) must be (0)
            val symbolsC = model.unresolvedSymbolForwardReferences(CasedSymbolName("C"))
            symbolsC must have size 1
            symbolsC.head.casedSymbolName.toString must be("A") // C still needed by A

            val symbolSet = model.getLabelsAndConstants.toSet
            symbolSet must be(Set(SymbolTableEntry(CasedSymbolName("B"), 3)))
        }

        logger.info("*** Defining C")
        // Now let's define C, then everything should exist, and there should be nothing in the unresolveable map.
        val cLine = IndexedLine(2, SourceLocation("", 3), "C EQU 4", None, Some(ConstantAssignment(new SymbolName("C"), Number(4))))
        model.setConstant(CasedSymbolName("C"), 4, cLine)
        logger.info("*** Checking state after defining C")
        if (true) { // just for fresh scope
            model.resolutionCount(CasedSymbolName("B")) must be (1)
            val symbolsB = model.unresolvedSymbolForwardReferences(CasedSymbolName("B"))
            symbolsB must have size 0 // B not needed by A any more, since A is a variable (their changes are not tracked after first definition)

            model.resolutionCount(CasedSymbolName("C")) must be (1)
            val symbolsC = model.unresolvedSymbolForwardReferences(CasedSymbolName("C"))
            symbolsC must have size 0 // C not needed by A any more, since A is a variable (their changes are not tracked after first definition)

            val symbolSet = model.getLabelsAndConstants.toSet
            symbolSet must be(Set(
                SymbolTableEntry(CasedSymbolName("B"), 3),
                SymbolTableEntry(CasedSymbolName("C"), 4)
            )) // A isn't in there, as it's a variable. Symbols (as shown on the Listing) are only Constants and Labels
            model.getVariable(CasedSymbolName("A")) must be(11)
        }
    }

    // Variable bulk get/reset -----------------------------------------------------------------------------------------

    @Test
    def getAllVariables(): Unit = {
        val vDollar = CasedSymbolName("$")
        val vOne = CasedSymbolName("V_ONE")
        val vTwo = CasedSymbolName("V_TWO")

        model.setVariable(vOne, 1, genDummyLine(0))
        model.setVariable(vTwo, 2, genDummyLine(1))
        model.getVariable(vOne) must be(1)
        model.getVariable(vTwo) must be(2)

        val cThree = CasedSymbolName("C_THREE")
        model.setConstant(cThree, 3, genDummyLine(2))
        model.getConstant(cThree) must be(3)

        val lFour = CasedSymbolName("L_FOUR")
        model.setLabel(lFour, 4, genDummyLine(3))
        model.getLabel(lFour) must be(4)

        val vars = model.getVariables()
        vars.size must be(3)
        logger.info("vars is " + vars)
        vars.keySet must contain theSameElementsAs(Set(vDollar, vOne, vTwo))
        vars.get(vDollar) must be(Some(0))
        vars.get(vOne) must be(Some(1))
        vars.get(vTwo) must be(Some(2))
    }

    @Test
    def resetVariablesSilently(): Unit = {
        val vChanges = CasedSymbolName("V_Changes")
        model.setVariable(vChanges, 1, genDummyLine(0))
        val vars = model.getVariables()

        model.setVariable(vChanges, 2, genDummyLine(1))
        model.getVariable(vChanges) must be(2)

        model.resetVariablesSilently(vars)
        model.getVariable(vChanges) must be(1)
    }

    // Constants -------------------------------------------------------------------------------------------------------

    @Test
    def unknownConstantRetrieval(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' has not been defined")
        model.getConstant(fnord)
    }

    @Test
    def constantCanBeSet(): Unit = {
        model.setConstant(fnord, 69, genDummyLine(1))
        model.getConstant(fnord) must be(69)
        model.constant(fnord) must be(Some(69))
        model.variable(fnord) must be(None) // it's a constant, not a variable, nor a label
        model.label(fnord) must be(None)
        model.getLabelsAndConstants must have size 1
        model.getLabelsAndConstants.head must be(SymbolTableEntry(fnord, 69))
    }

    @Test
    def constantNamesCaseInsensitivity(): Unit = {
        CasedSymbolName.setCaseSensitivity(false)

        model.setConstant(CasedSymbolName("fnord"), 69, genDummyLine(1))
        model.getConstant(CasedSymbolName("FNORD")) must be(69)
        model.getConstant(CasedSymbolName("fnord")) must be(69)
        model.getConstant(CasedSymbolName("FnORd")) must be(69)
        model.getLabelsAndConstants.head must be(SymbolTableEntry(CasedSymbolName("FNORD"), 69))
    }

    @Test
    def constantNamesCaseSensitivity(): Unit = {
        CasedSymbolName.setCaseSensitivity(true)

        try {
            model.setConstant(CasedSymbolName("fnord"), 69, genDummyLine(1))
            model.getConstant(CasedSymbolName("fnord")) must be(69)
            model.setConstant(CasedSymbolName("FnORd"), 42, genDummyLine(2))
            model.getConstant(CasedSymbolName("FnORd")) must be(42)
            model.getLabelsAndConstants.toSet must be(Set(
                SymbolTableEntry(CasedSymbolName("fnord"), 69),
                SymbolTableEntry(CasedSymbolName("FnORd"), 42)))
        } finally {
            CasedSymbolName.setCaseSensitivity(false)
        }
    }

    @Test
    def constantCannotBeRedefined(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot override existing constant; defined on line 1")

        model.setConstant(fnord, 69, genDummyLine(1))
        model.setConstant(fnord, 17, genDummyLine(2))
    }

    @Test
    def constantCanBeRedefinedInConvergeMode(): Unit = {
        model.setConstant(fnord, 69, genDummyLine(1))

        model.setConvergeMode(true)

        model.setConstant(fnord, 17, genDummyLine(2))

        model.getConstant(fnord) must be(17)
    }

    @Test
    def constantRedefinitionInConvergeModeRemembersNewDefinitionLine(): Unit = {
        model.setConstant(fnord, 69, genDummyLine(1))

        model.setConvergeMode(true)

        model.setConstant(fnord, 17, genDummyLine(2))

        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot override existing constant; defined on line 2")

        model.setConvergeMode(false)

        model.setConstant(fnord, 89, genDummyLine(3))
    }

    @Test
    def constantCannotBeDefinedOverExistingVariable(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot override existing variable; defined on line 1")

        model.setVariable(fnord, 69, genDummyLine(1))
        model.setConstant(fnord, 17, genDummyLine(2))
    }

    @Test
    def constantCannotBeDefinedOverExistingLabel(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot override existing label; defined on line 1")

        model.setLabel(fnord, 69, genDummyLine(1))
        model.setConstant(fnord, 17, genDummyLine(2))
    }

    @Test
    def constantCannotBeDefinedOverExistingVariableEvenInConvergeMode(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot override existing variable; defined on line 1")

        model.setConvergeMode(true)

        model.setVariable(fnord, 69, genDummyLine(1))
        model.setConstant(fnord, 17, genDummyLine(2))
    }

    @Test
    def constantCannotBeDefinedOverExistingLabelEvenInConvergeMode(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot override existing label; defined on line 1")

        model.setConvergeMode(true)

        model.setLabel(fnord, 69, genDummyLine(1))
        model.setConstant(fnord, 17, genDummyLine(2))
    }

    @Test
    def constantSetWithUnresolvedSymbolsGetsResolvedAsTheSymbolsAreDefined(): Unit = {
        // A EQU B + (C * 2)
        // where B and C are undefined
        val unresolvableExpr = Binary(Add(), SymbolArg("B"), Binary(Mult(), SymbolArg("C"), Number(2)))
        val line = IndexedLine(0, SourceLocation("", 1), "A EQU B + (C * 2)", None, Some(ConstantAssignment(new SymbolName("A"), unresolvableExpr)))
        // When codegen processes the EQU, it'll do...
        model.recordSymbolForwardReferences(Set(CasedSymbolName("B"), CasedSymbolName("C")), CasedSymbolName("A"), unresolvableExpr, line, SymbolType.Constant)
        // which will record A as an unresolvable symbol keyed by (i.e. resolvable when) B and C are defined.
        if (true) { // just for fresh scope
            val symbolsB = model.unresolvedSymbolForwardReferences(CasedSymbolName("B"))
            model.resolutionCount(CasedSymbolName("B")) must be (0)
            symbolsB must have size 1
            symbolsB.head.casedSymbolName.toString must be("A")
            symbolsB.head.indexedLine must be(line)
            symbolsB.head.symbolType must be(SymbolType.Constant)
            symbolsB.head.expr must be(unresolvableExpr)

            val symbolsC = model.unresolvedSymbolForwardReferences(CasedSymbolName("C"))
            symbolsC must have size 1
            symbolsC.head.casedSymbolName.toString must be("A")
            // won't bother checking the rest of the detail that was checked above..

            model.getLabelsAndConstants must be(empty)
        }

        // Now let's define B. Then only C will be unresolvable. B should exist, A and C should not.
        val bLine = IndexedLine(1, SourceLocation("", 2), "B EQU 3", None, Some(ConstantAssignment(new SymbolName("B"), Number(3))))
        model.setConstant(CasedSymbolName("B"), 3, bLine)
        if (true) { // just for fresh scope
            val symbolsB = model.unresolvedSymbolForwardReferences(CasedSymbolName("B"))
            model.resolutionCount(CasedSymbolName("B")) must be (1)
            symbolsB must have size 1 // but it has now been resolved; it is retained for change tracking

            val symbolsC = model.unresolvedSymbolForwardReferences(CasedSymbolName("C"))
            symbolsC must have size 1
            symbolsC.head.casedSymbolName.toString must be("A")

            val symbolSet = model.getLabelsAndConstants.toSet
            symbolSet must be(Set(SymbolTableEntry(CasedSymbolName("B"), 3)))
        }

        // Now let's define C, then everything should exist, and there should be nothing in the unresolveable map.
        val cLine = IndexedLine(2, SourceLocation("", 3), "C EQU 4", None, Some(ConstantAssignment(new SymbolName("C"), Number(4))))
        model.setConstant(CasedSymbolName("C"), 4, cLine)
        if (true) { // just for fresh scope
            val symbolsB = model.unresolvedSymbolForwardReferences(CasedSymbolName("B"))
            model.resolutionCount(CasedSymbolName("B")) must be (1)
            symbolsB must have size 1 // but it has now been resolved; it is retained for change tracking

            val symbolsC = model.unresolvedSymbolForwardReferences(CasedSymbolName("C"))
            model.resolutionCount(CasedSymbolName("C")) must be (1)
            symbolsC must have size 1 // but it has now been resolved; it is retained for change tracking

            val symbolSet = model.getLabelsAndConstants.toSet
            symbolSet must be(Set(
                SymbolTableEntry(CasedSymbolName("A"), 11),
                SymbolTableEntry(CasedSymbolName("B"), 3),
                SymbolTableEntry(CasedSymbolName("C"), 4)
            ))
            model.getConstant(CasedSymbolName("A")) must be(11)
        }
    }

    // Labels ----------------------------------------------------------------------------------------------------------
    @Test
    def unknownLabelRetrieval(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' has not been defined")
        model.getLabel(fnord)
    }

    @Test
    def labelCanBeSet(): Unit = {
        model.setLabel(fnord, 69, genDummyLine(1))
        model.getLabel(fnord) must be(69)
        model.label(fnord) must be(Some(69))
        model.variable(fnord) must be(None) // it's a label, not a variable, nor a constant (semantically, though it is constant)
        model.constant(fnord) must be(None)
        model.getLabelsAndConstants must have size 1
        model.getLabelsAndConstants.head must be(SymbolTableEntry(fnord, 69))
    }

    @Test
    def labelNamesCaseInsensitivity(): Unit = {
        CasedSymbolName.setCaseSensitivity(false)

        model.setLabel(CasedSymbolName("fnord"), 69, genDummyLine(1))
        model.getLabel(CasedSymbolName("FNORD")) must be(69)
        model.getLabel(CasedSymbolName("fnord")) must be(69)
        model.getLabel(CasedSymbolName("FnORd")) must be(69)
        model.getLabelsAndConstants.head must be(SymbolTableEntry(CasedSymbolName("FNORD"), 69))
    }

    @Test
    def labelNamesCaseSensitivity(): Unit = {
        CasedSymbolName.setCaseSensitivity(true)

        try {
            model.setLabel(CasedSymbolName("fnord"), 69, genDummyLine(1))
            model.getLabel(CasedSymbolName("fnord")) must be(69)
            model.setLabel(CasedSymbolName("FnORd"), 42, genDummyLine(2))
            model.getLabel(CasedSymbolName("FnORd")) must be(42)
            model.getLabelsAndConstants.toSet must be(Set(
                SymbolTableEntry(CasedSymbolName("fnord"), 69),
                SymbolTableEntry(CasedSymbolName("FnORd"), 42)))
        } finally {
            CasedSymbolName.setCaseSensitivity(false)
        }
    }

    @Test
    def labelCannotBeRedefined(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot override existing label; defined on line 1")

        model.setLabel(fnord, 69, genDummyLine(1))
        model.setLabel(fnord, 17, genDummyLine(2))
    }

    @Test
    def labelCanBeRedefinedInConvergeMode(): Unit = {
        model.setLabel(fnord, 69, genDummyLine(1))

        model.setConvergeMode(true)

        model.setLabel(fnord, 17, genDummyLine(2))

        model.getLabel(fnord) must be(17)
    }

    @Test
    def labelRedefinedInConvergeModeRemembersNewDefinitionLine(): Unit = {
        model.setLabel(fnord, 69, genDummyLine(1))

        model.setConvergeMode(true)

        model.setLabel(fnord, 17, genDummyLine(2))

        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot override existing label; defined on line 2")

        model.setConvergeMode(false)

        model.setLabel(fnord, 87, genDummyLine(3))
    }

    @Test
    def labelCannotBeDefinedOverExistingVariable(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot override existing variable; defined on line 1")

        model.setVariable(fnord, 17, genDummyLine(1))
        model.setLabel(fnord, 69, genDummyLine(2))
    }

    @Test
    def labelCannotBeDefinedOverExistingConstant(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot override existing constant; defined on line 1")

        model.setConstant(fnord, 17, genDummyLine(1))
        model.setLabel(fnord, 69, genDummyLine(2))
    }

    @Test
    def labelCannotBeDefinedOverExistingVariableEvenInConvergeMode(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot override existing variable; defined on line 1")

        model.setConvergeMode(true)

        model.setVariable(fnord, 17, genDummyLine(1))
        model.setLabel(fnord, 69, genDummyLine(2))
    }

    @Test
    def labelCannotBeDefinedOverExistingConstantEvenInConvergeMode(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot override existing constant; defined on line 1")

        model.setConvergeMode(true)

        model.setConstant(fnord, 17, genDummyLine(1))
        model.setLabel(fnord, 69, genDummyLine(2))
    }

    // Evaluate Expression ---------------------------------------------------------------------------------------------

    @Test
    def evalNumber(): Unit = {
        model.evaluateExpression(Number(12)) must be(Right(12))
    }

    @Test
    def evalSymbolArgOfExistingVariable(): Unit = {
        model.setVariable(fnord, 45, genDummyLine(0))
        model.evaluateExpression(fnordSymbolArg) must be(Right(45))
    }

    @Test
    def evalSymbolArgOfDollar(): Unit = {
        model.evaluateExpression(SymbolArg("$")) must be(Right(0))
    }

    @Test
    def evalSymbolArgOfUndefinedVariable(): Unit = {
        model.evaluateExpression(fnordSymbolArg) must be(Left(Set(fnord)))
    }

    @Test
    def evalMultipleUndefinedVariable(): Unit = {
        // A completely nonsense expression that could not be evaluated, but that contains all types of Expression
        // subclass.
        val expr = Binary(Add(), Binary(Add(), Unary(Negate(), fnordSymbolArg), SymbolArg("waaah")), Binary(Add(),
            SymbolArg("foo"), Binary(Add(), Characters("xxx"), Binary(Add(), Number(5), SymbolArg("bar")))))
        model.evaluateExpression(expr) must be(Left(Set(CasedSymbolName("waaah"), fnord, CasedSymbolName("bar"), foo)))
    }

    @Test
    def evalSymbolArgOfExistingConstant(): Unit = {
        model.setConstant(fnord, 45, genDummyLine(1))
        model.evaluateExpression(fnordSymbolArg) must be(Right(45))
    }

    @Test
    def evalSymbolArgOfExistingLabel(): Unit = {
        model.setLabel(fnord, 45, genDummyLine(1))
        model.evaluateExpression(fnordSymbolArg) must be(Right(45))
    }

    @Test
    def evalCharactersFails(): Unit = {
        // For db/dw/dd, we expand character expressions as they occur in the parameter list -
        // evaluateExpression only returns an Int. See allocateStorageForLine and expandCharacterExpressions.

        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Cannot evaluate 'Characters(FNORD)' as an Int")
        model.evaluateExpression(Characters(fnord.toString))
    }

    @Test
    def evalUnaryNegation(): Unit = {
        model.evaluateExpression(Unary(Negate(), Number(3))) must be(Right(-3))
        model.evaluateExpression(Unary(Negate(), Number(0))) must be(Right(0))
        model.evaluateExpression(Unary(Negate(), Number(-3))) must be(Right(3))
    }

    @Test
    def evalUnaryNot(): Unit = {
        // Not is not actually used in eForth!
        model.evaluateExpression(Unary(Not(), Number(1))) must be(Right(-2))
        model.evaluateExpression(Unary(Not(), Number(0))) must be(Right(-1))
        model.evaluateExpression(Unary(Not(), Number(-3))) must be(Right(2))
    }

    @Test
    def evalUnaryOffset(): Unit = {
        model.evaluateExpression(Unary(OffsetFrom(0x24), Number(0x24))) must be (Right(0))
        model.evaluateExpression(Unary(OffsetFrom(0x24), Number(0x20))) must be (Right(-4)) // expr before $
        model.evaluateExpression(Unary(OffsetFrom(0x24), Number(0x28))) must be (Right(4))  // expr after $
    }

    @Test
    def evalUnaryOffsetMustHaveBeenReplacedByOffsetFrom(): Unit = {
        thrown.expect(classOf[IllegalStateException])
        thrown.expectMessage("Offset should have been transformed to an OffsetFrom")

        model.evaluateExpression(Unary(Offset(), Number(0x24)))
    }

    @Test
    def evalBinaryAdd(): Unit = {
        model.evaluateExpression(Binary(Add(), Number(3), Number(4))) must be(Right(7))
    }

    @Test
    def evalBinarySub(): Unit = {
        model.evaluateExpression(Binary(Sub(), Number(8), Number(3))) must be(Right(5))
    }

    @Test
    def evalBinaryMult(): Unit = {
        model.evaluateExpression(Binary(Mult(), Number(8), Number(3))) must be(Right(24))
    }

    @Test
    def evalBinaryDiv(): Unit = {
        model.evaluateExpression(Binary(Div(), Number(8), Number(2))) must be(Right(4))
    }

    @Test
    def evalBinaryShiftLeft(): Unit = {
        model.evaluateExpression(Binary(ShiftLeft(), Number(8), Number(2))) must be(Right(32))
    }

    @Test
    def evalBinaryShiftRight(): Unit = {
        model.evaluateExpression(Binary(ShiftRight(), Number(16), Number(2))) must be(Right(4))
    }

    @Test
    def evalBinaryAnd(): Unit = {
        model.evaluateExpression(Binary(And(), Number(6), Number(3))) must be(Right(2))
    }

    @Test
    def evalBinaryOr(): Unit = {
        model.evaluateExpression(Binary(Or(), Number(6), Number(3))) must be(Right(7))
    }

    @Test
    def evalBinaryXor(): Unit = {
        model.evaluateExpression(Binary(Xor(), Number(7), Number(3))) must be(Right(4))
    }

    @Test
    def evalComplicated(): Unit = {
        // -2 + 3 * (10 / (10 >> 1)) == 4
        val shr = Binary(ShiftRight(), Number(10), Number(1))
        val div = Binary(Div(), Number(10), shr)
        val mult = Binary(Mult(), Number(3), div)
        val add = Binary(Add(), Unary(Negate(), Number(2)), mult)
        model.evaluateExpression(add) must be(Right(4))
    }

    // Undefined Symbols -----------------------------------------------------------------------------------------------

    @Test
    def undefinedSymbol(): Unit = {
        model.definedValue(fnord) must be(false)
    }

    @Test
    def definedVariable(): Unit = {
        model.setVariable(fnord, 1, genDummyLine(1))
        model.definedValue(fnord) must be(true)
    }

    @Test
    def definedConstant(): Unit = {
        model.setConstant(fnord, 1, genDummyLine(1))
        model.definedValue(fnord) must be(true)
    }

    @Test
    def definedLabel(): Unit = {
        model.setLabel(fnord, 1, genDummyLine(1))
        model.definedValue(fnord) must be(true)
    }

    @Test
    def findUndefinedSymbolInSymbolArg(): Unit = {
        model.findUndefineds(fnordSymbolArg) must be(Set(fnord))
        model.setConstant(fnord, 1, genDummyLine(1))
        model.findUndefineds(fnordSymbolArg) must be(empty)
    }

    @Test
    def findUndefinedSymbolInObviousEmpties(): Unit = {
        model.findUndefineds(Number(3)) must be(empty)
        model.findUndefineds(Characters("boo")) must be(empty)
    }

    @Test
    def findUndefinedSymbolInUnary(): Unit = {
        model.findUndefineds(Unary(Negate(), fnordSymbolArg)) must be(Set(fnord))
        model.setConstant(fnord, 1, genDummyLine(1))
        model.findUndefineds(Unary(Negate(), fnordSymbolArg)) must be(empty)
    }


    @Test
    def findUndefinedSymbolInBinary(): Unit = {
        model.findUndefineds(Binary(Add(), fnordSymbolArg, SymbolArg("foo"))) must be(Set(fnord, foo))
        model.setConstant(fnord, 1, genDummyLine(1))
        model.findUndefineds(Binary(Add(), fnordSymbolArg, SymbolArg("foo"))) must be(Set(foo))
        model.setConstant(foo, 1, genDummyLine(1))
        model.findUndefineds(Binary(Add(), fnordSymbolArg, SymbolArg("foo"))) must be(empty)
    }

    // Storage ---------------------------------------------------------------------------------------------------------

    @Test
    def storageRetrieval(): Unit = {
        val address = 69
        model.setDollar(address, genDummyLine(0))
        val exprs = List(Number(42), Number(69), Number(0), Number(1))
        val line = IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs)))
        val storage = model.allocateStorageForLine(line, 1, exprs)
        storage.data must be(Array(42, 69, 0, 1))
        storage.cellWidth must be(1)
        storage.address must be(address)
        storage.indexedLine must be(line)
    }

    @Test
    def noSourcedValueForLineGivesEmptyList(): Unit = {
        model.getSourcedValuesForLineIndex(17) must be(empty)
    }

    @Test
    def storageIncrementsDollarByCellWidthTimesLength(): Unit = {
        val startAddress = 20
        val cellWidth = 1
        model.setDollar(startAddress, genDummyLine(0))

        val exprs = List(Number(42), Number(69))
        val line = IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs)))
        model.allocateStorageForLine(line, cellWidth, exprs)

        model.getDollar must be(startAddress + (cellWidth * exprs.size))
    }

    @Test
    def storageWithForwardReferenceHasZeroesInItsData(): Unit = {
        val exprs = List(Number(42), fnordSymbolArg, SymbolArg("foo"), Number(96))
        val storage = model.allocateStorageForLine(IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs))), 1, exprs)

        storage.data must be(Array[Int](42, 0, 0, 96))
    }

    @Test
    def storageWithForwardReferenceIsRecordedForLaterFixup(): Unit = {
        val exprs = List(fnordSymbolArg, SymbolArg("foo"))
        val storage = model.allocateStorageForLine(IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs))), 1, exprs)

        model.storageForwardReferences(fnord) must be (Set(storage))
        model.storageForwardReferences(foo) must be (Set(storage))
    }

    @Test
    def storageWithForwardReferenceIsRemovedOnVariableDefinitionButOtherForwardReferencesRemain(): Unit = {
        val exprs = List(fnordSymbolArg, SymbolArg("foo"))
        val storage = model.allocateStorageForLine(IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs))), 1, exprs)
        model.setVariable(fnord, 73, genDummyLine(4))

        model.storageForwardReferences(fnord) must be (Set.empty)
        model.storageForwardReferences(foo) must be (Set(storage))
    }

    @Test
    def storageWithForwardReferenceIsFixedUpAndForwardReferenceRemovedOnVariableDefinition(): Unit = {
        val exprs = List(fnordSymbolArg)
        val storage = model.allocateStorageForLine(IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs))), 1, exprs)
        model.setVariable(fnord, 73, genDummyLine(4))

        storage.data must be(Array[Int](73))
        model.storageForwardReferences(fnord) must be (Set.empty)
    }

    @Test
    def storageWithForwardReferenceIsFixedUpAndForwardReferenceNotRemovedOnConstantDefinition(): Unit = {
        val exprs = List(fnordSymbolArg)
        val storage = model.allocateStorageForLine(IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs))), 1, exprs)
        model.setConstant(fnord, 73, genDummyLine(4))

        storage.data must be(Array[Int](73))
        model.storageForwardReferences(fnord) must contain (storage)
    }

    @Test
    def storageWithForwardReferenceIsFixedUpAndForwardReferenceNotRemovedOnLabelDefinition(): Unit = {
        val exprs = List(fnordSymbolArg)
        val storage = model.allocateStorageForLine(IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs))), 1, exprs)
        model.setLabel(fnord, 73, genDummyLine(4))

        storage.data must be(Array[Int](73))
        model.storageForwardReferences(fnord) must contain (storage)
    }

    @Test
    def storageOfCharactersIsExpandedToNumbers(): Unit = {
        val cellWidth = 1
        val str = "abc"
        val exprs = List(Characters(str))
        val line = IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs)))
        val storage = model.allocateStorageForLine(line, cellWidth, exprs)

        storage.data must be(Array[Int](97, 98, 99))
        model.getDollar must be(cellWidth * str.length)
    }

    @Test
    def assigmentOfValueByVariableForLine(): Unit = {
        model.setVariable(fnord, 42, genDummyLine(4))

        checkAssignmentOfValue(false)
    }

    @Test
    def assigmentOfValueByConstantForLine(): Unit = {
        model.setConstant(fnord, 42, genDummyLine(4))

        checkAssignmentOfValue(false)
    }

    @Test
    def assigmentOfValueByLabelForLine(): Unit = {
        model.setLabel(fnord, 42, genDummyLine(4))

        checkAssignmentOfValue(true)
    }

    private def checkAssignmentOfValue(isLabel: Boolean) = {
        var foundLine = 0
        var foundValue = 0
        var foundSymbolType = SymbolType.Variable

        // the only way to get AssignmentValues out
        model.foreachSourcedValue((lineIndex: Int, sourcedValues: List[SourcedValue]) => {
            for (sourcedValue <- sourcedValues) {
                val assignmentValue = sourcedValue.asInstanceOf[AssignmentValue]
                foundLine = lineIndex + 1 // assuming no macro expansion, inclusions
                foundValue = assignmentValue.data
                foundSymbolType = assignmentValue.symbolType
            }
        })
        foundLine must be(4)
        foundValue must be(42)
        if (isLabel) {
            foundSymbolType must be(SymbolType.Label)
        } else {
            foundSymbolType must not(be(SymbolType.Label))
        }
    }

    @Test
    def sourcedValuesCanBeCleared(): Unit = {
        val numbers = List(Number(1), Number(2), Number(3))
        val lineNumber = 4
        val lineIndex = 3
        val line = IndexedLine(lineIndex, SourceLocation("", lineNumber), "FNORD: DB 1,2,3", Some("FNORD"), Some(DB(numbers)))
        model.setLabel(fnord, 42, line)
        model.allocateStorageForLine(line, 1, numbers)

        val sourcedValues = model.getSourcedValuesForLineIndex(lineIndex)
        sourcedValues must have size 2
        for (sourcedValue <- sourcedValues) {
            logger.debug("SV is " + sourcedValue)
        }

        model.clearSourcedValuesForLineIndex(lineIndex)

        model.getSourcedValuesForLineIndex(lineIndex) must be('empty)
    }

    @Test
    def dbBounds(): Unit = {
        val exprs = List(Number(0), Number(255))
        val line = IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(exprs)))
        val storage = model.allocateStorageForLine(line, 1, exprs)

        storage.data must be(Array(0, 255))
    }

    @Test
    def dbOverflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of 256 cannot be expressed in a BYTE on line 7")

        val exprs = List(Number(256))
        val line = IndexedLine(6, SourceLocation("", 7), "irrelevant", None, Some(DB(exprs)))

        model.allocateStorageForLine(line, 1, exprs)
    }

    @Ignore
    @Test
    def dbUnderflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of -1 cannot be expressed in a BYTE on line 7")

        val exprs = List(Number(-1))
        val line = IndexedLine(6, SourceLocation("", 7), "irrelevant", None, Some(DB(exprs)))

        model.allocateStorageForLine(line, 1, exprs)
    }


    @Test
    def dwBounds(): Unit = {
        val exprs = List(Number(0), Number(65535))
        val line = IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DW(exprs)))
        val storage = model.allocateStorageForLine(line, 2, exprs)

        storage.data must be(Array(0, 65535))
    }

    @Test
    def dwOverflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of 65536 cannot be expressed in a WORD on line 7")

        val exprs = List(Number(65536))
        val line = IndexedLine(6, SourceLocation("", 7), "irrelevant", None, Some(DW(exprs)))

        model.allocateStorageForLine(line, 2, exprs)
    }

    @Ignore
    @Test
    def dwUnderflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of -1 cannot be expressed in a WORD on line 7")

        val exprs = List(Number(-1))
        val line = IndexedLine(6, SourceLocation("", 7), "irrelevant", None, Some(DW(exprs)))

        model.allocateStorageForLine(line, 2, exprs)
    }

    @Test
    def ddBounds(): Unit = {
        val exprs = List(Number(0), Number(0xffffffff))
        val line = IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DD(exprs)))
        val storage = model.allocateStorageForLine(line, 4, exprs)

        storage.data must be(Array(0, 0xffffffff))
    }

    // DD overflow cannot be expressed in the type system, since Storage uses Ints, and 0x100000000 overflows that.

    @Ignore
    @Test
    def ddUnderflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of -1 cannot be expressed in a DWORD on line 7")

        val exprs = List(Number(-1))
        val line = IndexedLine(6, SourceLocation("", 7), "irrelevant", None, Some(DD(exprs)))

        model.allocateStorageForLine(line, 4, exprs)
    }

    @Test
    def remainingForwardReferencesAtEndOfFirstPassCausesFailure(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Storage forward references remain unresolved at end of Pass 1: (AARDVARK: #1; FNORD: #3, #4; FOO: #5; ZYGOTE: #1)")

        val line1 = IndexedLine(0, SourceLocation("", 1), "irrelevant", None, Some(DB(List(SymbolArg("aardvark"), SymbolArg("zygote")))))
        model.allocateStorageForLine(line1, 1, List(SymbolArg("aardvark"), SymbolArg("zygote")))

        val line3 = IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(List(fnordSymbolArg))))
        model.allocateStorageForLine(line3, 1, List(fnordSymbolArg))

        val line4 = IndexedLine(3, SourceLocation("", 4), "irrelevant", None, Some(DB(List(fnordSymbolArg))))
        model.allocateStorageForLine(line4, 1, List(fnordSymbolArg))

        val line5 = IndexedLine(4, SourceLocation("", 5), "irrelevant", None, Some(DB(List(SymbolArg("foo")))))
        model.allocateStorageForLine(line5, 1, List(SymbolArg("foo")))

        model.checkUnresolvedForwardReferences()
    }

    @Test
    def noForwardReferencesAtEndOfFirstPassIsGood(): Unit = {
        val line = IndexedLine(2, SourceLocation("", 3), "irrelevant", None, Some(DB(List(fnordSymbolArg))))
        model.allocateStorageForLine(line, 1, List(fnordSymbolArg))
        model.setVariable(fnord, 34, genDummyLine(9))

        model.checkUnresolvedForwardReferences()
    }

    @Test
    def lowestAndHighestStorageAddresses(): Unit = {
        model.setDollar(0x4000, genDummyLine(1))

        val exprs = List(Number(1), Number(2))
        model.allocateStorageForLine(IndexedLine(1, SourceLocation("", 2), "", None, Some(DB(exprs))), 1, exprs)

        model.setDollar(0x4020, genDummyLine(3))

        model.allocateStorageForLine(IndexedLine(3, SourceLocation("", 4), "", None, Some(DB(exprs))), 1, exprs)

        model.lowestStorageAddress must be(0x4000)
        model.highestStorageAddress must be(0x4021)
    }

    @Test
    def foreachSourcedValueGivesLinesAndSourcedValueInOrder(): Unit = {
        simpleOrderingTestModel

        val lineIndices = mutable.ArrayBuffer[Int]()
        val storageBytes = mutable.ArrayBuffer[Int]()
        val assignmentValues = mutable.ArrayBuffer[Int]()
        var calls = 0
        model.foreachSourcedValue((lineIndex: Int, sourcedValues: List[SourcedValue]) => {
            calls += 1
            lineIndices += lineIndex
            for (sourcedValue <- sourcedValues) {
                sourcedValue match {
                    case storage: Storage =>
                        for (data <- storage.data) {
                            storageBytes += data
                        }
                    case assignmentValue: AssignmentValue =>
                        assignmentValues += assignmentValue.data
                }
            }
        })

        // two line 2's are coalesced into one call with two Storages
        calls must be(4)

        lineIndices.toArray must be(Array(0, 1, 3, 4))
        storageBytes.toArray must be(Array(1, 2, 3, 21))
        assignmentValues.toArray must be(Array(42))
    }

    private def simpleOrderingTestModel = {
        // The Lines here are illustrative of the Lines that would cause the allocateStorageForLine/assignmentForLine
        // calls...
        val line5 = IndexedLine(4, SourceLocation("", 5), "irrelevant", None, Some(DB(List(Number(21)))))
        model.allocateStorageForLine(line5, 1, List(Number(21)))

        // e.g. val line4 = Line(SourceLocation("", 4), "irrelevant", None, Some(VariableAssignment(new SymbolName(fnord), Number(42))))
        model.setVariable(fnord, 42, genDummyLine(4))

        val line1 = IndexedLine(0, SourceLocation("", 1), "irrelevant", None, Some(DB(List(Number(1)))))
        model.allocateStorageForLine(line1, 1, List(Number(1)))

        val line2 = IndexedLine(1, SourceLocation("", 2), "irrelevant", None, Some(DB(List(Number(2)))))
        model.allocateStorageForLine(line2, 1, List(Number(2)))

        val line2again = IndexedLine(1, SourceLocation("", 2), "irrelevant", None, Some(DB(List(Number(3)))))
        model.allocateStorageForLine(line2again, 1, List(Number(3)))
    }

    @Test
    def allocateInstructionStorageForLine(): Unit = {
        val bytes = List(0x29, 0x4c, 0x2a, 0xfb)
        val line5 = IndexedLine(4, SourceLocation("", 5), "fpuclrerr", None, Some(IndirectInstruction("FPUCLRERR", bytes)))
        val storage = model.allocateInstructionStorageForLine(line5,  bytes)
        storage.cellWidth must be(1)
        storage.address must be(0)
        storage.exprs must be (bytes map { Number }) // for completeness

        val builder = new mutable.ArrayBuffer[Int]()
        builder.++=(bytes)
        storage.data must be(builder.result())
    }
}
