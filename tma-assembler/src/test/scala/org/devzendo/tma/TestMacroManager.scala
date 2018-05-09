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

import org.devzendo.tma.ast.AST.{MacroArgument, MacroName, MacroParameterName}
import org.devzendo.tma.ast.MacroDefinition
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestMacroManager extends AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    val macroManager = new MacroManager(true)
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

    @Test
    def macroDoesNotExist(): Unit = { // won't happen in the parser, since each macro word is checked for presence
        thrown.expect(classOf[IllegalStateException])
        thrown.expectMessage("Macro '" + macroName + "' does not exist")

        macroManager.expandMacro(macroName, List.empty)
    }

    private def setupSampleNonNestedMacro = {
        // Exhaustively testing each delimiter is done in the 'punctuation' tests; a few are illustrated here
        macroManager.startMacro(macroName, macroParameterNames)
        macroManager.addMacroLine("Replaced: FOO: (BAR) (FOO) FOO-BAR FOO,BAR <FOO> {FOO} @FOO")
        macroManager.addMacroLine("Kept: foo (foo) *bar* _FOO _foo FOOD MYFOOD XFOO FOOFOO FOOBAR")
        macroManager.endMacro()
    }

    @Test
    def macroCalledWithMoreArgumentsThanParameters(): Unit = {
        thrown.expect(classOf[IllegalStateException])
        thrown.expectMessage("Macro '" + macroName + "' has 2 parameters, but is called with 3")

        setupSampleNonNestedMacro
        macroManager.expandMacro(macroName, List(new MacroArgument("1"), new MacroArgument("replacement"), new MacroArgument("dodgy")))
    }

    @Test
    def macroInvocationReplacesParametersWithArgumentsHonouringCaseOfParameters(): Unit = {
        setupSampleNonNestedMacro

        val lines = macroManager.expandMacro(macroName, List(new MacroArgument("1"), new MacroArgument("replacement")))
        lines must be(List(
            "Replaced: 1: (replacement) (1) 1-replacement 1,replacement <1> {1} @1",
            "Kept: foo (foo) *bar* _FOO _foo FOOD MYFOOD XFOO FOOFOO FOOBAR"
        ))
    }

    @Test
    def macroInvocationWithSameParametersAsArgumentsEffectivelyDoesNothing(): Unit = {
        setupSampleNonNestedMacro

        val lines = macroManager.expandMacro(macroName, List(new MacroArgument("FOO"), new MacroArgument("BAR")))
        lines must be(List(
            "Replaced: FOO: (BAR) (FOO) FOO-BAR FOO,BAR <FOO> {FOO} @FOO",
            "Kept: foo (foo) *bar* _FOO _foo FOOD MYFOOD XFOO FOOFOO FOOBAR"
        ))
    }

    @Test
    def macroInvocationWithFewerArgumentsThanParametersFillInWithEmptySpace(): Unit = {
        setupSampleNonNestedMacro

        val lines = macroManager.expandMacro(macroName, List(new MacroArgument("1")))
        lines must be(List(
            "Replaced: 1: () (1) 1- 1, <1> {1} @1",
            "Kept: foo (foo) *bar* _FOO _foo FOOD MYFOOD XFOO FOOFOO FOOBAR"
        ))
    }

    @Test
    def macroInvocationWithNoArgumentsFillsInAllParametersWithEmptySpace(): Unit = {
        setupSampleNonNestedMacro

        val lines = macroManager.expandMacro(macroName, List.empty)
        lines must be(List(
            "Replaced: : () () - , <> {} @",
            "Kept: foo (foo) *bar* _FOO _foo FOOD MYFOOD XFOO FOOFOO FOOBAR"
        ))
    }

    // A macro with a parameter called PARAM is expanded with an argument of YES. Does PARAM get replaced with YES for
    // a given macro body, containing PARAM surrounded by various other characters (which may or may not be macro-
    // delimiting characters), at start and end of line.
    def checkParameterReplacementDelimiter(macroBody: String, expectedExpansion: String): Unit = {
        // Since this may be called in a loop, it would redefine the macro in the instance-variable macroManager, so
        // create a new MacroManager inside here..
        val localMacroManager = new MacroManager(true)
        localMacroManager.startMacro(macroName, List(new MacroParameterName("PARAM")))
        localMacroManager.addMacroLine(macroBody)
        localMacroManager.endMacro()
        val strings = localMacroManager.expandMacro(macroName, List(new MacroArgument("YES")))
        strings must be(List(expectedExpansion))
    }

    @Test
    def macroParameterReplacementStartOfLine(): Unit = {
        checkParameterReplacementDelimiter("PARAM ; start of line", "YES ; start of line")
    }

    @Test
    def macroParameterReplacementStartOfLineWithLeadingWhitespace(): Unit = {
        checkParameterReplacementDelimiter(" PARAM ; start of line", " YES ; start of line")
    }

    @Test
    def macroParameterReplacementEndOfLine(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB PARAM", "LLL: DB YES")
    }

    @Test
    def macroParameterReplacementEndOfLineWithTrailingWhitespace(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB PARAM ", "LLL: DB YES ")
    }

    @Test
    def macroParameterReplacementStartAndEndOfLine(): Unit = {
        checkParameterReplacementDelimiter("PARAM", "YES")
    }

    @Test
    def macroParameterReplacementSurroundingWhitespace(): Unit = {
        checkParameterReplacementDelimiter(" PARAM ", " YES ")
    }

    @Test
    def macroParameterReplacementPrecedingComma(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB 5,PARAM", "LLL: DB 5,YES")
    }

    @Test
    def macroParameterReplacementSucceedingComma(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB PARAM,5", "LLL: DB YES,5")
    }

    @Test
    def macroParameterReplacementPrecedingNumber(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB 5PARAM", "LLL: DB 5PARAM")
        // syntactically wrong, but that's not for the macro manager to determine
    }

    @Test
    def macroParameterReplacementSuccedingNumber(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB PARAM4", "LLL: DB PARAM4")
        // syntactically wrong, but that's not for the macro manager to determine
    }

    @Test
    def macroParameterReplacementSurroundedByNumbers(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB 3PARAM4", "LLL: DB 3PARAM4")
        // syntactically wrong, but that's not for the macro manager to determine
    }

    @Test
    def macroParameterReplacementPrecedingLetter(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB XPARAM", "LLL: DB XPARAM")
    }

    @Test
    def macroParameterReplacementSuccedingLetter(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB PARAMX", "LLL: DB PARAMX")
    }

    @Test
    def macroParameterReplacementSurroundedByLetters(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB YPARAMX", "LLL: DB YPARAMX")
    }

    @Test
    def macroParameterReplacementPrecedingUnderscore(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB _PARAM", "LLL: DB _PARAM")
    }

    @Test
    def macroParameterReplacementSuccedingUnderscore(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB PARAM_", "LLL: DB PARAM_")
    }

    @Test
    def macroParameterReplacementSurroundedByUnderscores(): Unit = {
        checkParameterReplacementDelimiter("LLL: DB _PARAM_", "LLL: DB _PARAM_")
    }

    val punctuation = ",.<>/?;:{}[]|!@#$%^&*()-+="

    @Test
    def macroParameterReplacementPrecedingPunctuation(): Unit = {
        for (c <- punctuation) {
            checkParameterReplacementDelimiter("LLL: DB " + c + "PARAM", "LLL: DB " + c + "YES")
        }
    }

    @Test
    def macroParameterReplacementSuccedingPunctuation(): Unit = {
        for (c <- punctuation) {
            checkParameterReplacementDelimiter("LLL: DB PARAM" + c, "LLL: DB YES" + c)
        }
    }

    @Test
    def macroParameterReplacementSurroundedByPunctuation(): Unit = {
        for (c <- punctuation) {
            checkParameterReplacementDelimiter("LLL: DB " + c + "PARAM" + c, "LLL: DB " + c + "YES" + c)
        }
    }

    @Test
    def macrosCannotHaveDuplicatedParameterName(): Unit = {
        thrown.expect(classOf[IllegalStateException])
        thrown.expectMessage("Macro '" + macroName + "' has duplicated parameter names")

        macroManager.startMacro(macroName, List(new MacroParameterName("SAME"), new MacroParameterName("SAME")))
    }
}
