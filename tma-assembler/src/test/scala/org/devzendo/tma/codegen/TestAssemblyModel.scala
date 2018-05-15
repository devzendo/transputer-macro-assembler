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

import org.junit.{Ignore, Rule, Test}
import org.junit.rules.ExpectedException
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestAssemblyModel extends AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger
    val dollar = "$"

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val model = new AssemblyModel

    @Test
    def unknownVariableRetrieval(): Unit = {
        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("Variable 'FNORD' has not been defined")
        model.getVariable("FNORD")
    }

    @Test
    def dollarIsAlwaysPresentAndInitialised(): Unit = {
        model.getVariable(dollar) must be(0)
        model.getDollar() must be(0)
    }

    @Test
    def dollarCanBeSet(): Unit = {
        model.setDollar(50)
        model.getDollar() must be(50)
    }

    @Test
    def variableCanBeSet(): Unit = {
        model.setVariable("FNORD", 69)
        model.getVariable("FNORD") must be(69)
    }

    // TODO loads of evaluateExpression tests
}
