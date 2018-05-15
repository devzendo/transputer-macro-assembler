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

import org.devzendo.tma.ast.AST.Label
import org.devzendo.tma.ast._
import org.junit.{Ignore, Rule, Test}
import org.junit.rules.ExpectedException
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestCodeGenerator extends AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger
    val dollar = "$"

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val codegen = new CodeGenerator(true)

    @Test
    def initialConditions(): Unit = {
        val model = codegen.createModel(List.empty)
        model.title must be("")
        model.rows must be(25)
        model.columns must be(80)
        model.processor must be(None)
        model.getDollar() must be(0)
        model.getVariable(dollar) must be (0)
    }

    private def generateFromStatement(stmt: Statement): AssemblyModel = {
        generateFromLine(Line(1, "", None, Some(stmt)))
    }

    private def generateFromLabelledStatement(label: Label, stmt: Statement): AssemblyModel = {
        generateFromLine(Line(1, "", Some(label), Some(stmt)))
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
    def processor(): Unit = {
        generateFromStatement(Processor("T800")).processor must be(Some("T800"))
    }

    @Test
    def ignored(): Unit = {
        generateFromStatement(Ignored())
        // nothing to test, it just doesn't throw...
    }

    @Ignore
    @Test
    def unknownVariableRetrieval(): Unit = {
        // TODO need to do Org, which doesn't permit forward references, first.

        thrown.expect(classOf[AssemblyModelException])
        thrown.expectMessage("1: Variable 'FNORD' has not been defined")
        generateFromStatement(Org(SymbolArg("FNORD")))
    }

    @Test
    def orgNumber(): Unit = {
        val model = generateFromStatement(Org(Number(42)))
        model.getDollar() must be(42)
        model.getVariable(dollar) must be(42)
    }
}
