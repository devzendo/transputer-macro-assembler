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
import org.junit.rules.ExpectedException
import org.junit.{Ignore, Rule, Test}
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestAssemblyModel extends AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger
    val dollar = "$"
    val fnord = "FNORD"

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val model = new AssemblyModel


    @Test
    def initialConditions(): Unit = {
        model.hasEndBeenSeen must be (false)
    }

    // Variables -------------------------------------------------------------------------------------------------------

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
        model.setDollar(50, 0)
        model.getDollar must be(50)
        model.setVariable(dollar, 23, 0)
        model.getDollar must be(23)
        model.getVariable(dollar) must be(23)
    }

    @Test
    def variableCanBeSet(): Unit = {
        model.setVariable(fnord, 69, 0)
        model.getVariable(fnord) must be(69)
        model.variable(fnord) must be(Some(69))
        model.constant(fnord) must be(None) // it's a variable, not a constant, nor a label
        model.label(fnord) must be(None)
    }

    @Test
    def variableCanBeRedefined(): Unit = {
        model.setVariable(fnord, 32, 0)
        model.setVariable(fnord, 69, 0)
        model.getVariable(fnord) must be(69)
    }

    @Test
    def variableCannotBeDefinedOverExistingConstant(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Variable '" + fnord + "' cannot override existing constant; initially defined on line 1")

        model.setConstant(fnord, 69, 1)
        model.setVariable(fnord, 17, 2)
    }

    @Test
    def variableCannotBeDefinedOverExistingLabel(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Variable '" + fnord + "' cannot override existing label; initially defined on line 1")

        model.setLabel(fnord, 69, 1)
        model.setVariable(fnord, 17, 2)
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
        model.setConstant(fnord, 69, 1)
        model.getConstant(fnord) must be(69)
        model.constant(fnord) must be(Some(69))
        model.variable(fnord) must be(None) // it's a constant, not a variable, nor a label
        model.label(fnord) must be(None)
    }

    @Test
    def constantCannotBeRedefined(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot be redefined; initially defined on line 1")

        model.setConstant(fnord, 69, 1)
        model.setConstant(fnord, 17, 2)
    }

    @Test
    def constantCannotBeDefinedOverExistingVariable(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot override existing variable; last stored on line 1")

        model.setVariable(fnord, 69, 1)
        model.setConstant(fnord, 17, 2)
    }

    @Test
    def constantCannotBeDefinedOverExistingLabel(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Constant '" + fnord + "' cannot override existing label; initially defined on line 1")

        model.setLabel(fnord, 69, 1)
        model.setConstant(fnord, 17, 2)
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
        model.setLabel(fnord, 69, 1)
        model.getLabel(fnord) must be(69)
        model.label(fnord) must be(Some(69))
        model.variable(fnord) must be(None) // it's a label, not a variable, nor a constant (semantically, though it is constant)
        model.constant(fnord) must be(None)
    }

    @Test
    def labelCannotBeRedefined(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot be redefined; initially defined on line 1")

        model.setLabel(fnord, 69, 1)
        model.setLabel(fnord, 17, 2)
    }

    @Test
    def labelCannotBeDefinedOverExistingVariable(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot override existing variable; last stored on line 1")

        model.setVariable(fnord, 17, 1)
        model.setLabel(fnord, 69, 2)
    }

    @Test
    def labelCannotBeDefinedOverExistingConstant(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Label '" + fnord + "' cannot override existing constant; initially defined on line 1")

        model.setConstant(fnord, 17, 1)
        model.setLabel(fnord, 69, 2)
    }

    // Evaluate Expression ---------------------------------------------------------------------------------------------

    @Test
    def evalNumber(): Unit = {
        model.evaluateExpression(Number(12)) must be(Right(12))
    }

    @Test
    def evalSymbolArgOfExistingVariable(): Unit = {
        model.setVariable(fnord, 45, 0)
        model.evaluateExpression(SymbolArg(fnord)) must be(Right(45))
    }

    @Test
    def evalSymbolArgOfDollar(): Unit = {
        model.evaluateExpression(SymbolArg("$")) must be(Right(0))
    }

    @Test
    def evalSymbolArgOfUndefinedVariable(): Unit = {
        model.evaluateExpression(SymbolArg(fnord)) must be(Left(Set(fnord)))
    }

    @Test
    def evalMultipleUndefinedVariable(): Unit = {
        // A completely nonsense expression that could not be evaluated, but that contains all types of Expression
        // subclass.
        val expr = Binary(Add(), Binary(Add(), Unary(Negate(), SymbolArg(fnord)), SymbolArg("waaah")), Binary(Add(),
            SymbolArg("foo"), Binary(Add(), Characters("xxx"), Binary(Add(), Number(5), SymbolArg("bar")))))
        model.evaluateExpression(expr) must be(Left(Set("waaah", fnord, "bar", "foo")))
    }

    @Test
    def evalSymbolArgOfExistingConstant(): Unit = {
        model.setConstant(fnord, 45, 1)
        model.evaluateExpression(SymbolArg(fnord)) must be(Right(45))
    }

    @Test
    def evalSymbolArgOfExistingLabel(): Unit = {
        model.setLabel(fnord, 45, 1)
        model.evaluateExpression(SymbolArg(fnord)) must be(Right(45))
    }

    @Test
    def evalCharactersFails(): Unit = {
        // For db/dw/dd, we'll have to take care of character expressions as they occur in the parameter list -
        // evaluateExpression only returns an Int.

        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Cannot evaluate 'Characters(FNORD)' as an Int")
        model.evaluateExpression(Characters(fnord))
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
        model.definedValue(new SymbolName(fnord)) must be(false)
    }

    @Test
    def definedVariable(): Unit = {
        model.setVariable(fnord, 1, 1)
        model.definedValue(new SymbolName(fnord)) must be(true)
    }

    @Test
    def definedConstant(): Unit = {
        model.setConstant(fnord, 1, 1)
        model.definedValue(new SymbolName(fnord)) must be(true)
    }

    @Test
    def definedLabel(): Unit = {
        model.setLabel(fnord, 1, 1)
        model.definedValue(new SymbolName(fnord)) must be(true)
    }

    @Test
    def findUndefinedSymbolInSymbolArg(): Unit = {
        model.findUndefineds(SymbolArg(fnord)) must be(Set(fnord))
        model.setConstant(fnord, 1, 1)
        model.findUndefineds(SymbolArg(fnord)) must be(empty)
    }

    @Test
    def findUndefinedSymbolInObviousEmpties(): Unit = {
        model.findUndefineds(Number(3)) must be(empty)
        model.findUndefineds(Characters("boo")) must be(empty)
    }

    @Test
    def findUndefinedSymbolInUnary(): Unit = {
        model.findUndefineds(Unary(Negate(), SymbolArg(fnord))) must be(Set(fnord))
        model.setConstant(fnord, 1, 1)
        model.findUndefineds(Unary(Negate(), SymbolArg(fnord))) must be(empty)
    }

    @Test
    def findUndefinedSymbolInBinary(): Unit = {
        model.findUndefineds(Binary(Add(), SymbolArg(fnord), SymbolArg("foo"))) must be(Set(fnord, "foo"))
        model.setConstant(fnord, 1, 1)
        model.findUndefineds(Binary(Add(), SymbolArg(fnord), SymbolArg("foo"))) must be(Set("foo"))
        model.setConstant("foo", 1, 1)
        model.findUndefineds(Binary(Add(), SymbolArg(fnord), SymbolArg("foo"))) must be(empty)
    }

    // Storage ---------------------------------------------------------------------------------------------------------

    @Test
    def storageRetrieval(): Unit = {
        val address = 69
        model.setDollar(address, 0)
        val exprs = List(Number(42), Number(69), Number(0), Number(1))
        val line = Line(3, "irrelevant", None, Some(DB(exprs)))
        val storage = model.allocateStorageForLine(line, 1, exprs)
        storage.data must be(Array(42, 69, 0, 1))
        storage.cellWidth must be(1)
        storage.address must be(address)
        storage.line must be(line)
    }

    @Test
    def noStorageForLineGivesEmptyList(): Unit = {
        model.getStoragesForLine(17) must be(empty)
    }

    @Test
    def storageIncrementsDollarByCellWidthTimesLength(): Unit = {
        val startAddress = 20
        val cellWidth = 1
        model.setDollar(startAddress, 0)

        val exprs = List(Number(42), Number(69))
        val line = Line(3, "irrelevant", None, Some(DB(exprs)))
        model.allocateStorageForLine(line, cellWidth, exprs)

        model.getDollar must be(startAddress + (cellWidth * exprs.size))
    }

    @Test
    def storageWithForwardReferenceHasZeroesInItsData(): Unit = {
        val exprs = List(Number(42), SymbolArg(fnord), SymbolArg("foo"), Number(96))
        val storage = model.allocateStorageForLine(Line(3, "irrelevant", None, Some(DB(exprs))), 1, exprs)

        storage.data must be(Array[Int](42, 0, 0, 96))
    }

    @Test
    def storageWithForwardReferenceIsRecordedForLaterFixup(): Unit = {
        val exprs = List(SymbolArg(fnord), SymbolArg("foo"))
        val storage = model.allocateStorageForLine(Line(3, "irrelevant", None, Some(DB(exprs))), 1, exprs)

        model.forwardReferences(fnord) must be (Set(storage))
        model.forwardReferences("foo") must be (Set(storage))
    }

    @Test
    def storageWithForwardReferenceIsRemovedOnVariableDefinitionButOtherForwardReferencesRemain(): Unit = {
        val exprs = List(SymbolArg(fnord), SymbolArg("foo"))
        val storage = model.allocateStorageForLine(Line(3, "irrelevant", None, Some(DB(exprs))), 1, exprs)
        model.setVariable(fnord, 73, 4)

        model.forwardReferences(fnord) must be (Set.empty)
        model.forwardReferences("foo") must be (Set(storage))
    }

    @Test
    def storageWithForwardReferenceIsFixedUpAndForwardReferenceRemovedOnVariableDefinition(): Unit = {
        val exprs = List(SymbolArg(fnord))
        val storage = model.allocateStorageForLine(Line(3, "irrelevant", None, Some(DB(exprs))), 1, exprs)
        model.setVariable(fnord, 73, 4)

        storage.data must be(Array[Int](73))
        model.forwardReferences(fnord) must be (Set.empty)
    }

    @Test
    def storageWithForwardReferenceIsFixedUpAndForwardReferenceRemovedOnConstantDefinition(): Unit = {
        val exprs = List(SymbolArg(fnord))
        val storage = model.allocateStorageForLine(Line(3, "irrelevant", None, Some(DB(exprs))), 1, exprs)
        model.setConstant(fnord, 73, 4)

        storage.data must be(Array[Int](73))
        model.forwardReferences(fnord) must be (Set.empty)
    }

    @Test
    def storageWithForwardReferenceIsFixedUpAndForwardReferenceRemovedOnLabelDefinition(): Unit = {
        val exprs = List(SymbolArg(fnord))
        val storage = model.allocateStorageForLine(Line(3, "irrelevant", None, Some(DB(exprs))), 1, exprs)
        model.setLabel(fnord, 73, 4)

        storage.data must be(Array[Int](73))
        model.forwardReferences(fnord) must be (Set.empty)
    }

    @Test
    def undefinedCountExpressionInDupFails(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Count of 'SymbolArg(FNORD)' is undefined on line 3")

        model.allocateStorageForLine(Line(3, "irrelevant", None, Some(DBDup(SymbolArg(fnord), Number(5)))), 1, SymbolArg(fnord), Number(5))
    }

    @Test
    def storageOfDbDup(): Unit = {
        val address = 69
        model.setDollar(address, 0)
        val line = Line(3, "irrelevant", None, Some(DBDup(Number(5), Number(69))))
        val storage = model.allocateStorageForLine(line, 1, Number(5), Number(69))
        storage.data must be(Array(69, 69, 69, 69, 69))
        storage.cellWidth must be(1)
        storage.address must be(address)
        storage.line must be(line)
    }

    @Test
    def storageOfDbDupIncrementsAddress(): Unit = {
        val address = 69
        model.setDollar(address, 0)
        val count = 5
        val cellWidth = 1
        val line = Line(3, "irrelevant", None, Some(DBDup(Number(count), Number(69))))
        model.allocateStorageForLine(line, 1, Number(count), Number(69))
        model.getDollar must be (address + (count * cellWidth))
    }

    @Test
    def storageOfDbDupWithForwardReferenceIsRecordedForLaterFixup(): Unit = {
        val line = Line(3, "irrelevant", None, Some(DBDup(Number(5), SymbolArg(fnord))))
        val storage = model.allocateStorageForLine(line, 1, Number(5), SymbolArg(fnord))
        model.forwardReferences(fnord) must be (Set(storage))
    }

    @Test
    def storageOfDbDupWithForwardReferenceIsFixedUpAndForwardReferenceRemovedOnVariableDefinition(): Unit = {
        val line = Line(3, "irrelevant", None, Some(DBDup(Number(5), SymbolArg(fnord))))
        val storage = model.allocateStorageForLine(line, 1, Number(5), SymbolArg(fnord))
        model.setVariable(fnord, 73, 4)

        storage.data must be(Array[Int](73, 73, 73, 73, 73))
        model.forwardReferences(fnord) must be (Set.empty)
    }

    @Test
    def dbBounds(): Unit = {
        val exprs = List(Number(0), Number(255))
        val line = Line(3, "irrelevant", None, Some(DB(exprs)))
        val storage = model.allocateStorageForLine(line, 1, exprs)

        storage.data must be(Array(0, 255))
    }

    @Test
    def dbOverflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of 256 cannot be expressed in a BYTE on line 7")

        val exprs = List(Number(256))
        val line = Line(7, "irrelevant", None, Some(DB(exprs)))

        model.allocateStorageForLine(line, 1, exprs)
    }

    @Ignore
    @Test
    def dbUnderflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of -1 cannot be expressed in a BYTE on line 7")

        val exprs = List(Number(-1))
        val line = Line(7, "irrelevant", None, Some(DB(exprs)))

        model.allocateStorageForLine(line, 1, exprs)
    }


    @Test
    def dwBounds(): Unit = {
        val exprs = List(Number(0), Number(65535))
        val line = Line(3, "irrelevant", None, Some(DW(exprs)))
        val storage = model.allocateStorageForLine(line, 2, exprs)

        storage.data must be(Array(0, 65535))
    }

    @Test
    def dwOverflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of 65536 cannot be expressed in a WORD on line 7")

        val exprs = List(Number(65536))
        val line = Line(7, "irrelevant", None, Some(DW(exprs)))

        model.allocateStorageForLine(line, 2, exprs)
    }

    @Ignore
    @Test
    def dwUnderflow(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Value of -1 cannot be expressed in a WORD on line 7")

        val exprs = List(Number(-1))
        val line = Line(7, "irrelevant", None, Some(DW(exprs)))

        model.allocateStorageForLine(line, 2, exprs)
    }

    @Test
    def ddBounds(): Unit = {
        val exprs = List(Number(0), Number(0xffffffff))
        val line = Line(3, "irrelevant", None, Some(DD(exprs)))
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
        val line = Line(7, "irrelevant", None, Some(DD(exprs)))

        model.allocateStorageForLine(line, 4, exprs)
    }

    @Test
    def remainingForwardReferencesAtEndOfFirstPassCausesFailure(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Forward references remain unresolved at end of Pass 1: (aardvark: #1; FNORD: #3, #4; foo: #5; zygote: #1)")

        val line1 = Line(1, "irrelevant", None, Some(DB(List(SymbolArg("aardvark"), SymbolArg("zygote")))))
        model.allocateStorageForLine(line1, 1, List(SymbolArg("aardvark"), SymbolArg("zygote")))

        val line3 = Line(3, "irrelevant", None, Some(DBDup(Number(5), SymbolArg(fnord))))
        model.allocateStorageForLine(line3, 1, Number(5), SymbolArg(fnord))

        val line4 = Line(4, "irrelevant", None, Some(DB(List(SymbolArg(fnord)))))
        model.allocateStorageForLine(line4, 1, List(SymbolArg(fnord)))

        val line5 = Line(5, "irrelevant", None, Some(DB(List(SymbolArg("foo")))))
        model.allocateStorageForLine(line5, 1, List(SymbolArg("foo")))

        model.checkUnresolvedForwardReferences()
    }

    @Test
    def noForwardReferencesAtEndOfFirstPassIsGood(): Unit = {
        val line = Line(3, "irrelevant", None, Some(DBDup(Number(5), SymbolArg(fnord))))
        model.allocateStorageForLine(line, 1, Number(5), SymbolArg(fnord))
        model.setVariable(fnord, 34, 9)

        model.checkUnresolvedForwardReferences()
    }
}
