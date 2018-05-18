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

import org.devzendo.tma.ast._
import org.junit.{Ignore, Rule, Test}
import org.junit.rules.ExpectedException
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

    // TODO db dup - should ensure that the repeat value is a number or constant - need to know how big the storage
    // will be
}
