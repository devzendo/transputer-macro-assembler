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

import org.devzendo.tma.ast.{Number, SymbolArg}
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
        model.setDollar(50)
        model.getDollar must be(50)
        model.setVariable(dollar, 23)
        model.getDollar must be(23)
        model.getVariable(dollar) must be(23)
    }

    @Test
    def variableCanBeSet(): Unit = {
        model.setVariable(fnord, 69)
        model.getVariable(fnord) must be(69)
    }


    @Test
    def evalNumber(): Unit = {
        model.evaluateExpression(Number(12)) must be(Right(12))
    }

    @Test
    def evalSymbolArgOfExistingVariable(): Unit = {
        model.setVariable(fnord, 45)
        model.evaluateExpression(SymbolArg(fnord)) must be(Right(45))
    }

    @Test
    def evalSymbolArgOfDollar(): Unit = {
        model.evaluateExpression(SymbolArg("$")) must be(Right(0))
    }

    @Test
    def evalSymbolArgOfUndefinedVariable(): Unit = {
        model.evaluateExpression(SymbolArg(fnord)) must be(Left(List(fnord)))
    }

}
