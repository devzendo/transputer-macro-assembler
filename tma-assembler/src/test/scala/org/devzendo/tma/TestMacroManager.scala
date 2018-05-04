/*
 * Copyright (C) 2008-2017 Matt Gumbley, DevZendo.org http://devzendo.org
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

import org.devzendo.tma.ast.AST.{MacroParameterName, MacroName}
import org.devzendo.tma.ast.MacroDefinition
import org.junit.{Rule, Test}
import org.junit.rules.ExpectedException
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestMacroManager extends AssertionsForJUnit with MustMatchers {
    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val macroManager = new MacroManager()
    val macroName = new MacroName("name")
    val macroParameterNames = List(new MacroParameterName("FOO"), new MacroParameterName("BAR"))

    @Test
    def initialState(): Unit = {
        macroManager.isInMacroBody must be(false)
    }


    @Test
    def startMacroChangesIsInMacroBody(): Unit = {
        macroManager.startMacro(macroName, List.empty)
        macroManager.isInMacroBody must be(true)
    }

    @Test
    def endMacroChangesIsInMacroBody(): Unit = {
        macroManager.startMacro(macroName, List.empty)
        macroManager.endMacro()
        macroManager.isInMacroBody must be(false)
    }

    @Test
    def endMacroWhenNotInMacroBodyThrows(): Unit = {
        thrown.expect(classOf[IllegalStateException])
        thrown.expectMessage("End macro with no start macro")
        macroManager.endMacro()
    }

    @Test
    def endMacroStores(): Unit = {
        macroManager.getMacro(macroName) must be(None)
        macroManager.isInMacroBody must be(false)

        macroManager.startMacro(macroName, macroParameterNames)
        macroManager.isInMacroBody must be(true)
        macroManager.getMacro(macroName) must be(None)

        macroManager.endMacro()
        macroManager.isInMacroBody must be(false)

        macroManager.getMacro(macroName) must be(Some(MacroDefinition(macroName, macroParameterNames, List.empty)))
    }

    @Test
    def emptyMacrosAreAllowedButPointless(): Unit = {
        macroManager.startMacro(macroName, macroParameterNames)
        macroManager.endMacro()

        val expectedEmptyMacroLines = List.empty
        macroManager.getMacro(macroName) must be(Some(MacroDefinition(macroName, macroParameterNames, expectedEmptyMacroLines)))
    }

    @Test
    def addMacroLineWhenNotInMacroBodyThrows(): Unit = {
        thrown.expect(classOf[IllegalStateException])
        thrown.expectMessage("Macro line received with no start macro")
        macroManager.addMacroLine("blah")
    }

    @Test
    def macrosCanHaveLines(): Unit = {
        macroManager.startMacro(macroName, macroParameterNames)
        macroManager.addMacroLine("LINE ONE")
        macroManager.addMacroLine("LINE TWO")
        macroManager.endMacro()
        macroManager.getMacro(macroName) must be(Some(MacroDefinition(macroName, macroParameterNames,
            List("LINE ONE", "LINE TWO"))))
    }

    @Test
    def subsequentMacrosHaveTheirOwnLines(): Unit = {
        macroManager.startMacro(macroName, macroParameterNames)
        macroManager.addMacroLine("LINE ONE")
        macroManager.addMacroLine("LINE TWO")
        macroManager.endMacro()

        val secondMacroName = new MacroName("SECOND")
        macroManager.startMacro(secondMacroName, macroParameterNames)
        macroManager.addMacroLine("COMPLETELY")
        macroManager.addMacroLine("DIFFERENT")
        macroManager.endMacro()
        macroManager.getMacro(secondMacroName) must be(Some(MacroDefinition(secondMacroName, macroParameterNames,
            List("COMPLETELY", "DIFFERENT"))))
    }

    @Test
    def macrosCannotBeRedefined(): Unit = {
        thrown.expect(classOf[IllegalStateException])
        thrown.expectMessage("Macro '" + macroName + "' already defined")

        macroManager.startMacro(macroName, macroParameterNames)
        macroManager.endMacro()

        macroManager.startMacro(macroName, macroParameterNames)
    }

    @Test
    def macrosCanBeTestedForExistence(): Unit = {
        macroManager.exists(macroName) must be(false)

        macroManager.startMacro(macroName, macroParameterNames)
        macroManager.endMacro()

        macroManager.exists(macroName) must be(true)
    }

}
