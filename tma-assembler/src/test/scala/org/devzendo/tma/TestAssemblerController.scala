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

package org.devzendo.tma

import org.devzendo.tma.ast._
import org.devzendo.tma.codegen.{CodeGenerationException, CodeGenerator}
import org.devzendo.tma.parser.{AssemblyParser, AssemblyParserException, MacroManager}
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestAssemblerController extends AssertionsForJUnit with MustMatchers {

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val macroManager = new MacroManager(true)
    val parser = new AssemblyParser(true, true, macroManager)
    val codegen = new CodeGenerator(true)
    val controller = new AssemblerController(macroManager, parser, codegen)

    @Test
    def parserErrorsAccumulate(): Unit = {
        controller.parseTextLine(1, "blarg") // unknown command
        controller.parseTextLine(2, "fnord") // unknown command
        controller.parseTextLine(3, "bigmac macro") // ok
        controller.parseTextLine(4, "endm") // ok
        controller.parseTextLine(5, "bigmac macro") // duplicate macro
        val errors = controller.getParseExceptions.map((e: AssemblyParserException) => e.getMessage )
        errors must be(List(
            "1: Unknown statement 'blarg'",
            "2: Unknown statement 'fnord'",
            "5: Macro 'bigmac' already defined"
        ))
    }

    @Test
    def noParsedLinesCantGenerate(): Unit = {
        thrown.expect(classOf[RuntimeException])
        thrown.expectMessage("No parsed input")

        // no input lines added or parsed...

        controller.generateModel()
    }

    @Test
    def parseExceptionsCantGenerate(): Unit = {
        thrown.expect(classOf[RuntimeException])
        thrown.expectMessage("Parse errors; cannot continue")

        controller.parseTextLine(1, "") // perfectly valid empty line, ensures there is at least one parsed line
        controller.parseTextLine(2, "blarg") // unknown command

        controller.generateModel()
    }

    @Test // indirect, avoids mocks, and the interface extraction they'd necessitate just to test.
    def generate(): Unit = {
        controller.parseTextLine(1, "title 'hello world'")
        controller.parseTextLine(2, "end")
        // not much of a program! tests that the parsed lines are passed to codegen though..
        val model = controller.generateModel()
        model.title must be("'hello world'")
    }

    @Test // indirect, avoids mocks, and the interface extraction they'd necessitate just to test.
    def callsEndCheck(): Unit = {
        controller.parseTextLine(1, "title 'hello world'")
        // NO end statement
        // not much of a program! tests that the end check is called on codegen though..
        controller.generateModel()
        controller.getCodeGenerationExceptions.head.getMessage must be("1: End of input reached with no End statement")
    }

    @Test
    def codeGenerationErrorsAccumulate(): Unit = {
        // no parse errors here, but will cause code gen / model exceptions
        controller.addParsedLine(Line(1, "", None, Some(Org(Characters("blah"))))) // characters as an Org argument
        controller.addParsedLine(Line(2, "", None, Some(Org(SymbolArg("fnorg"))))) // fnord is undefined
        controller.addParsedLine(Line(3, "", None, Some(ConstantAssignment("bar", Characters("foo"))))) // cannot set a constant to characters
        controller.addParsedLine(Line(4, "", None, Some(ConstantAssignment("valid", Number(5))))) // perfectly valid
        controller.addParsedLine(Line(5, "", Some("valid"), Some(DB(List(Number(5)))))) // label can't override constant...
        // ... AssemblyModelException converted to CodeGenerationException
        controller.addParsedLine(Line(6, "", None, Some(VariableAssignment("unres", SymbolArg("missing"))))) // unresolved forward reference
        controller.addParsedLine(Line(7, "", None, Some(End(None))))
        controller.addParsedLine(Line(8, "", None, Some(ConstantAssignment("valid", Number(5))))) // no statements allowed after End

        controller.generateModel()

        val errors = controller.getCodeGenerationExceptions.map((e: CodeGenerationException) => e.getMessage )
        errors must be(List(
            "1: Origin cannot be set to a Character expression 'Characters(blah)'",
            "2: Undefined symbol(s) 'fnorg'",
            "3: Constant cannot be set to a Character expression 'Characters(foo)'",
            "5: Label 'VALID' cannot override existing constant; initially defined on line 4",
            "6: Variable cannot be set to an undefined symbol 'Set(missing)'",
            "8: No statements allowed after End statement"
        ))
    }
}
