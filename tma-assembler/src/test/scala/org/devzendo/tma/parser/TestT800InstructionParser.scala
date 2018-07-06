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

package org.devzendo.tma.parser

import org.devzendo.tma.ast.{DirectInstruction, IndirectInstruction, Number, SymbolArg}
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestT800InstructionParser extends AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger

    private val FOO = SymbolArg("FOO")

    class InstructionParser extends T800InstructionParser {
        // Because T800InstructionParser is now a trait, I need this class to extend it, so I can instantiate it.
    }
    val parser = new InstructionParser()
    parser.setDebugParser(true)
    parser.setLineNumber(1)

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    @Test
    def j(): Unit = {
        parser.parse("j 45") must be(DirectInstruction("J", 0, Number(45)))
    }


    @Test
    def ldlp(): Unit = {
        parser.parse("ldlp FOO") must be(DirectInstruction("LDLP", 1, FOO))
    }

    @Test
    def pfix(): Unit = {
        parser.parse("pfix 17") must be(DirectInstruction("PFIX", 2, Number(17)))
    }

    @Test
    def ldnl(): Unit = {
        parser.parse("ldnl FOO") must be(DirectInstruction("LDNL", 3, FOO))
    }

    @Test
    def ldc(): Unit = {
        parser.parse("ldc FOO") must be(DirectInstruction("LDC", 4, FOO))
    }

    @Test
    def ldnlp(): Unit = {
        parser.parse("ldNlP FOO") must be(DirectInstruction("LDNLP", 5, FOO))
    }

    @Test
    def nfix(): Unit = {
        parser.parse("nfix -3") must be(DirectInstruction("NFIX", 6, Number(-3)))
    }

    @Test
    def ldl(): Unit = {
        parser.parse("ldl 5") must be(DirectInstruction("LDL", 7, Number(5)))
    }

    @Test
    def adc(): Unit = {
        parser.parse("adc FOO") must be(DirectInstruction("ADC", 8, FOO))
    }

    @Test
    def call(): Unit = {
        parser.parse("Call FOO") must be(DirectInstruction("CALL", 9, FOO)) // mixed case too
    }

    @Test
    def cj(): Unit = {
        parser.parse("cJ FOO") must be(DirectInstruction("CJ", 10, FOO))
    }

    @Test
    def ajw(): Unit = {
        parser.parse("ajw FOO") must be(DirectInstruction("AJW", 11, FOO))
    }

    @Test
    def eqc(): Unit = {
        parser.parse("eqc 5") must be(DirectInstruction("EQC", 12, Number(5)))
    }

    @Test
    def stl(): Unit = {
        parser.parse("stl FOO") must be(DirectInstruction("STL", 13, FOO))
    }

    @Test
    def stnl(): Unit = {
        parser.parse("stnl FOO") must be(DirectInstruction("STNL", 14, FOO))
    }

    @Test
    def opr(): Unit = {
        parser.parse("opr 0x28") must be(DirectInstruction("OPR", 15, Number(0x28))) // will be expanded to pfix 02 opr 08
    }

}
