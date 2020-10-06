/*
 * Copyright (C) 2008-2020 Matt Gumbley, DevZendo.org http://devzendo.org
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
import org.devzendo.tma.codegen.{AssemblyModel, CasedSymbolName, CodeGenerationException, CodeGenerator}
import org.devzendo.tma.parser.{AssemblyParser, AssemblyParserException, MacroManager}
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestAssemblerController extends AssertionsForJUnit with MustMatchers {

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val includer = new SourceIncludingReader
    val macroManager = new MacroManager(true)
    val parser = new AssemblyParser(true, true, macroManager, includer)
    val model = new AssemblyModel(true)
    val codegen = new CodeGenerator(true, model)
    val controller = new AssemblerController(includer, parser, codegen)

    @Test
    def parserErrorsAccumulate(): Unit = {
        val file = "test.asm"
        controller.parseTextLine(SourceLocation(file, 1), "blarg") // unknown command
        controller.parseTextLine(SourceLocation(file, 2), "fnord") // unknown command
        controller.parseTextLine(SourceLocation(file, 3), "bigmac macro") // ok
        controller.parseTextLine(SourceLocation(file, 4), "endm") // ok
        controller.parseTextLine(SourceLocation(file, 5), "bigmac macro") // duplicate macro
        val errors = controller.getParseExceptions.map((e: AssemblyParserException) => e.getMessage )
        errors must be(List(
            "test.asm:1: Unknown statement 'blarg'",
            "test.asm:2: Unknown statement 'fnord'",
            "test.asm:5: Macro 'bigmac' already defined"
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

        controller.parseTextLine(SourceLocation("", 1), "") // perfectly valid empty line, ensures there is at least one parsed line
        controller.parseTextLine(SourceLocation("", 2), "blarg") // unknown command

        controller.generateModel()
    }

    @Test // indirect, avoids mocks, and the interface extraction they'd necessitate just to test.
    def generate(): Unit = {
        controller.parseTextLine(SourceLocation("", 1), "title 'hello world'")
        controller.parseTextLine(SourceLocation("", 2), "end")
        // not much of a program! tests that the parsed lines are passed to codegen though..
        val model = controller.generateModel()
        model.title must be("'hello world'")
    }

    @Test // indirect, avoids mocks, and the interface extraction they'd necessitate just to test.
    def callsEndCheck(): Unit = {
        controller.parseTextLine(SourceLocation("", 1), "title 'hello world'")
        // NO end statement
        // not much of a program! tests that the end check is called on codegen though..
        controller.generateModel()
        controller.getCodeGenerationExceptions.head.getMessage must be("1: End of input reached with no End statement")
    }

    @Test
    def codeGenerationErrorsAccumulate(): Unit = {
        // Precondition
        CasedSymbolName.caseSensitivity must be (false)

        // no parse errors here, but will cause code gen / model exceptions
        controller.addParsedLine(Line(SourceLocation("", 1), "", None, Some(Org(Characters("blah"))))) // characters as an Org argument
        controller.addParsedLine(Line(SourceLocation("", 2), "", None, Some(Org(SymbolArg("fnorg"))))) // fnorg is undefined
        controller.addParsedLine(Line(SourceLocation("", 3), "", None, Some(ConstantAssignment("bar", Characters("foo"))))) // cannot set a constant to characters
        controller.addParsedLine(Line(SourceLocation("", 4), "", None, Some(ConstantAssignment("valid", Number(5))))) // perfectly valid
        controller.addParsedLine(Line(SourceLocation("", 5), "", Some("valid"), Some(DB(List(Number(5)))))) // label can't override constant...
        // ... AssemblyModelException converted to CodeGenerationException
        controller.addParsedLine(Line(SourceLocation("", 6), "", None, Some(VariableAssignment("unres", SymbolArg("missing"))))) // unresolved forward reference
        controller.addParsedLine(Line(SourceLocation("", 7), "", None, Some(End(None))))
        controller.addParsedLine(Line(SourceLocation("", 8), "", None, Some(ConstantAssignment("valid", Number(5))))) // no statements allowed after End

        controller.generateModel()

        val errors = controller.getCodeGenerationExceptions.map((e: CodeGenerationException) => e.getMessage )
        errors must be(List(
            "1: Origin cannot be set to a Character expression 'Characters(blah)'",
            "2: Undefined symbol(s) 'FNORG'", // case insensitive symbols
            "3: Constant cannot be set to a Character expression 'Characters(foo)'",
            "5: Label 'VALID' cannot override existing constant; defined on line 4",
            "8: No statements allowed after End statement",
            "0: Symbol forward references remain unresolved at end of Pass 1: (MISSING: #6)"
        ))
    }
}
