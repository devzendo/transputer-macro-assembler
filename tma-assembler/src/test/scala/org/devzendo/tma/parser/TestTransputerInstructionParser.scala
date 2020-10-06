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

package org.devzendo.tma.parser

import org.devzendo.tma.SourceLocation
import org.devzendo.tma.ast.{DirectInstruction, IndirectInstruction, Number, SymbolArg}
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestTransputerInstructionParser extends AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger

    private val FOO = SymbolArg("FOO")

    class InstructionParser extends TransputerInstructionParser {
        // Because TransputerInstructionParser is now a trait, I need this class to extend it, so I can instantiate it.
    }
    val parser = new InstructionParser()
    parser.setDebugParser(true)
    parser.setSourceLocation(SourceLocation("", 1))

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    // -----------------------------------------------------------------------------------------------------------------

    @Test
    def j(): Unit = {
        parser.parse("j 45") must be(DirectInstruction("J", 0x00, Number(45)))
    }


    @Test
    def ldlp(): Unit = {
        parser.parse("ldlp FOO") must be(DirectInstruction("LDLP", 0x10, FOO))
    }

    @Test
    def pfix(): Unit = {
        parser.parse("pfix 17") must be(DirectInstruction("PFIX", 0x20, Number(17)))
    }

    @Test
    def ldnl(): Unit = {
        parser.parse("ldnl FOO") must be(DirectInstruction("LDNL", 0x30, FOO))
    }

    @Test
    def ldc(): Unit = {
        parser.parse("ldc FOO") must be(DirectInstruction("LDC", 0x40, FOO))
    }

    @Test
    def ldnlp(): Unit = {
        parser.parse("ldNlP FOO") must be(DirectInstruction("LDNLP", 0x50, FOO))
    }

    @Test
    def nfix(): Unit = {
        parser.parse("nfix -3") must be(DirectInstruction("NFIX", 0x60, Number(-3)))
    }

    @Test
    def ldl(): Unit = {
        parser.parse("ldl 5") must be(DirectInstruction("LDL", 0x70, Number(5)))
    }

    @Test
    def adc(): Unit = {
        parser.parse("adc FOO") must be(DirectInstruction("ADC", 0x80, FOO))
    }

    @Test
    def call(): Unit = {
        parser.parse("Call FOO") must be(DirectInstruction("CALL", 0x90, FOO)) // mixed case too
    }

    @Test
    def cj(): Unit = {
        parser.parse("cJ FOO") must be(DirectInstruction("CJ", 0xa0, FOO))
    }

    @Test
    def ajw(): Unit = {
        parser.parse("ajw FOO") must be(DirectInstruction("AJW", 0xb0, FOO))
    }

    @Test
    def eqc(): Unit = {
        parser.parse("eqc 5") must be(DirectInstruction("EQC", 0xc0, Number(5)))
    }

    @Test
    def stl(): Unit = {
        parser.parse("stl FOO") must be(DirectInstruction("STL", 0xd0, FOO))
    }

    @Test
    def stnl(): Unit = {
        parser.parse("stnl FOO") must be(DirectInstruction("STNL", 0xe0, FOO))
    }

    @Test
    def opr(): Unit = {
        parser.parse("opr 0x28") must be(DirectInstruction("OPR", 0xf0, Number(0x28))) // will be expanded to pfix 02 opr 08
    }

    // -----------------------------------------------------------------------------------------------------------------

    private def indirect(name: String, bytes: List[Int]): Unit = {
        parser.parse(name) must be(IndirectInstruction(name.toUpperCase, bytes))
    }

    @Test
    def rev(): Unit = {
        indirect("rev", List(0xf0))
    }

    @Test
    def lb(): Unit = {
        indirect("lb", List(0xf1))
    }

    @Test
    def bsub(): Unit = {
        indirect("bsub", List(0xf2))
    }

    @Test
    def endp(): Unit = {
        indirect("endp", List(0xf3))
    }

    @Test
    def diff(): Unit = {
        indirect("diff", List(0xf4))
    }

    @Test
    def add(): Unit = {
        indirect("add", List(0xf5))
    }

    @Test
    def gcall(): Unit = {
        indirect("gcall", List(0xf6))
    }

    @Test
    def in(): Unit = {
        indirect("in", List(0xf7))
    }

    @Test
    def prod(): Unit = {
        indirect("prod", List(0xf8))
    }

    @Test
    def gt(): Unit = {
        indirect("gt", List(0xf9))
    }

    @Test
    def wsub(): Unit = {
        indirect("wsub", List(0xfa))
    }

    @Test
    def out(): Unit = {
        indirect("out", List(0xfb))
    }

    @Test
    def sub(): Unit = {
        indirect("sub", List(0xfc))
    }

    @Test
    def startp(): Unit = {
        indirect("startp", List(0xfd))
    }

    @Test
    def outbyte(): Unit = {
        indirect("outbyte", List(0xfe))
    }

    @Test
    def outword(): Unit = {
        indirect("outword", List(0xff))
    }

    @Test
    def seterr(): Unit = {
        indirect("seterr", List(0x21, 0xf0))
    }

    @Test
    def resetch(): Unit = {
        indirect("resetch", List(0x21, 0xf2))
    }

    @Test
    def csub0(): Unit = {
        indirect("csub0", List(0x21, 0xf3))
    }

    @Test
    def stopp(): Unit = {
        indirect("stopp", List(0x21, 0xf5))
    }

    @Test
    def ladd(): Unit = {
        indirect("ladd", List(0x21, 0xf6))
    }

    @Test
    def stlb(): Unit = {
        indirect("stlb", List(0x21, 0xf7))
    }

    @Test
    def sthf(): Unit = {
        indirect("sthf", List(0x21, 0xf8))
    }

    @Test
    def norm(): Unit = {
        indirect("norm", List(0x21, 0xf9))
    }

    @Test
    def ldiv(): Unit = {
        indirect("ldiv", List(0x21, 0xfa))
    }

    @Test
    def ldpi(): Unit = {
        indirect("ldpi", List(0x21, 0xfb))
    }

    @Test
    def stlf(): Unit = {
        indirect("stlf", List(0x21, 0xfc))
    }

    @Test
    def xdble(): Unit = {
        indirect("xdble", List(0x21, 0xfd))
    }

    @Test
    def ldpri(): Unit = {
        indirect("ldpri", List(0x21, 0xfe))
    }

    @Test
    def rem(): Unit = {
        indirect("rem", List(0x21, 0xff))
    }

    @Test
    def ret(): Unit = {
        indirect("ret", List(0x22, 0xf0))
    }

    @Test
    def lend(): Unit = {
        indirect("lend", List(0x22, 0xf1))
    }

    @Test
    def ldtimer(): Unit = {
        indirect("ldtimer", List(0x22, 0xf2))
    }

    @Test
    def testerr(): Unit = {
        indirect("testerr", List(0x22, 0xf9))
    }

    @Test
    def testpranal(): Unit = {
        indirect("testpranal", List(0x22, 0xfa))
    }

    @Test
    def tin(): Unit = {
        indirect("tin", List(0x22, 0xfb))
    }

    @Test
    def div(): Unit = {
        indirect("div", List(0x22, 0xfc))
    }

    @Test
    def dist(): Unit = {
        indirect("dist", List(0x22, 0xfe))
    }

    @Test
    def disc(): Unit = {
        indirect("disc", List(0x22, 0xff))
    }

    @Test
    def diss(): Unit = {
        indirect("diss", List(0x23, 0xf0))
    }

    @Test
    def lmul(): Unit = {
        indirect("lmul", List(0x23, 0xf1))
    }

    @Test
    def _not(): Unit = {
        indirect("not", List(0x23, 0xf2))
    }

    @Test
    def xor(): Unit = {
        indirect("xor", List(0x23, 0xf3))
    }

    @Test
    def bcnt(): Unit = {
        indirect("bcnt", List(0x23, 0xf4))
    }

    @Test
    def lshr(): Unit = {
        indirect("lshr", List(0x23, 0xf5))
    }

    @Test
    def lshl(): Unit = {
        indirect("lshl", List(0x23, 0xf6))
    }

    @Test
    def lsum(): Unit = {
        indirect("lsum", List(0x23, 0xf7))
    }

    @Test
    def lsub(): Unit = {
        indirect("lsub", List(0x23, 0xf8))
    }

    @Test
    def runp(): Unit = {
        indirect("runp", List(0x23, 0xf9))
    }

    @Test
    def xword(): Unit = {
        indirect("xword", List(0x23, 0xfa))
    }

    @Test
    def sb(): Unit = {
        indirect("sb", List(0x23, 0xfb))
    }

    @Test
    def gajw(): Unit = {
        indirect("gajw", List(0x23, 0xfc))
    }

    @Test
    def savel(): Unit = {
        indirect("savel", List(0x23, 0xfd))
    }

    @Test
    def saveh(): Unit = {
        indirect("saveh", List(0x23, 0xfe))
    }

    @Test
    def wcnt(): Unit = {
        indirect("wcnt", List(0x23, 0xff))
    }

    @Test
    def shr(): Unit = {
        indirect("shr", List(0x24, 0xf0))
    }

    @Test
    def shl(): Unit = {
        indirect("shl", List(0x24, 0xf1))
    }

    @Test
    def mint(): Unit = {
        indirect("mint", List(0x24, 0xf2))
    }

    @Test
    def alt(): Unit = {
        indirect("alt", List(0x24, 0xf3))
    }

    @Test
    def altwt(): Unit = {
        indirect("altwt", List(0x24, 0xf4))
    }

    @Test
    def altend(): Unit = {
        indirect("altend", List(0x24, 0xf5))
    }

    @Test
    def and(): Unit = {
        indirect("and", List(0x24, 0xf6))
    }

    @Test
    def enbt(): Unit = {
        indirect("enbt", List(0x24, 0xf7))
    }

    @Test
    def enbc(): Unit = {
        indirect("enbc", List(0x24, 0xf8))
    }

    @Test
    def enbs(): Unit = {
        indirect("enbs", List(0x24, 0xf9))
    }

    @Test
    def move(): Unit = {
        indirect("move", List(0x24, 0xfa))
    }

    @Test
    def or(): Unit = {
        indirect("or", List(0x24, 0xfb))
    }

    @Test
    def csngl(): Unit = {
        indirect("csngl", List(0x24, 0xfc))
    }

    @Test
    def ccnt1(): Unit = {
        indirect("ccnt1", List(0x24, 0xfd))
    }

    @Test
    def talt(): Unit = {
        indirect("talt", List(0x24, 0xfe))
    }

    @Test
    def ldiff(): Unit = {
        indirect("ldiff", List(0x24, 0xff))
    }

    @Test
    def sthb(): Unit = {
        indirect("sthb", List(0x25, 0xf0))
    }

    @Test
    def taltwt(): Unit = {
        indirect("taltwt", List(0x25, 0xf1))
    }

    @Test
    def sum(): Unit = {
        indirect("sum", List(0x25, 0xf2))
    }

    @Test
    def mul(): Unit = {
        indirect("mul", List(0x25, 0xf3))
    }

    @Test
    def sttimer(): Unit = {
        indirect("sttimer", List(0x25, 0xf4))
    }

    @Test
    def stoperr(): Unit = {
        indirect("stoperr", List(0x25, 0xf5))
    }

    @Test
    def cword(): Unit = {
        indirect("cword", List(0x25, 0xf6))
    }

    @Test
    def clrhalterr(): Unit = {
        indirect("clrhalterr", List(0x25, 0xf7))
    }

    @Test
    def sethalterr(): Unit = {
        indirect("sethalterr", List(0x25, 0xf8))
    }

    @Test
    def testhalterr(): Unit = {
        indirect("testhalterr", List(0x25, 0xf9))
    }

    @Test
    def dup(): Unit = {
        indirect("dup", List(0x25, 0xfa))
    }

    @Test
    def move2dinit(): Unit = {
        indirect("move2dinit", List(0x25, 0xfb))
    }

    @Test
    def move2dall(): Unit = {
        indirect("move2dall", List(0x25, 0xfc))
    }

    @Test
    def move2dnonzero(): Unit = {
        indirect("move2dnonzero", List(0x25, 0xfd))
    }

    @Test
    def move2dzero(): Unit = {
        indirect("move2dzero", List(0x25, 0xfe))
    }

    @Test
    def unpacksn(): Unit = {
        indirect("unpacksn", List(0x26, 0xf3))
    }

    @Test
    def postnormsn(): Unit = {
        indirect("postnormsn", List(0x26, 0xfc))
    }

    @Test
    def roundsn(): Unit = {
        indirect("roundsn", List(0x26, 0xfd))
    }

    @Test
    def ldinf(): Unit = {
        indirect("ldinf", List(0x27, 0xf1))
    }

    @Test
    def fmul(): Unit = {
        indirect("fmul", List(0x27, 0xf2))
    }

    @Test
    def cflerr(): Unit = {
        indirect("cflerr", List(0x27, 0xf3))
    }

    @Test
    def crcword(): Unit = {
        indirect("crcword", List(0x27, 0xf4))
    }

    @Test
    def crcbyte(): Unit = {
        indirect("crcbyte", List(0x27, 0xf5))
    }

    @Test
    def bitcnt(): Unit = {
        indirect("bitcnt", List(0x27, 0xf6))
    }

    @Test
    def bitrevword(): Unit = {
        indirect("bitrevword", List(0x27, 0xf7))
    }

    @Test
    def bitrevnbits(): Unit = {
        indirect("bitrevnbits", List(0x27, 0xf8))
    }

    @Test
    def wsubdb(): Unit = {
        indirect("wsubdb", List(0x28, 0xf1))
    }

    @Test
    def fpldnldbi(): Unit = {
        indirect("fpldnldbi", List(0x28, 0xf2))
    }

    @Test
    def fpchkerr(): Unit = {
        indirect("fpchkerr", List(0x28, 0xf3))
    }

    @Test
    def fpstnldb(): Unit = {
        indirect("fpstnldb", List(0x28, 0xf4))
    }

    @Test
    def fpldnlsni(): Unit = {
        indirect("fpldnlsni", List(0x28, 0xf6))
    }

    @Test
    def fpadd(): Unit = {
        indirect("fpadd", List(0x28, 0xf7))
    }

    @Test
    def fpstnlsn(): Unit = {
        indirect("fpstnlsn", List(0x28, 0xf8))
    }

    @Test
    def fpsub(): Unit = {
        indirect("fpsub", List(0x28, 0xf9))
    }

    @Test
    def fpldnldb(): Unit = {
        indirect("fpldnldb", List(0x28, 0xfa))
    }

    @Test
    def fpmul(): Unit = {
        indirect("fpmul", List(0x28, 0xfb))
    }

    @Test
    def fpdiv(): Unit = {
        indirect("fpdiv", List(0x28, 0xfc))
    }

    @Test
    def fpldnlsn(): Unit = {
        indirect("fpldnlsn", List(0x28, 0xfe))
    }

    @Test
    def fpremfirst(): Unit = {
        indirect("fpremfirst", List(0x28, 0xff))
    }

    @Test
    def fpremstep(): Unit = {
        indirect("fpremstep", List(0x29, 0xf0))
    }

    @Test
    def fpnan(): Unit = {
        indirect("fpnan", List(0x29, 0xf1))
    }

    @Test
    def fpordered(): Unit = {
        indirect("fpordered", List(0x29, 0xf2))
    }

    @Test
    def fpnotfinite(): Unit = {
        indirect("fpnotfinite", List(0x29, 0xf3))
    }

    @Test
    def fpgt(): Unit = {
        indirect("fpgt", List(0x29, 0xf4))
    }

    @Test
    def fpeq(): Unit = {
        indirect("fpeq", List(0x29, 0xf5))
    }

    @Test
    def fpi32tor32(): Unit = {
        indirect("fpi32tor32", List(0x29, 0xf6))
    }

    @Test
    def fpi32tor64(): Unit = {
        indirect("fpi32tor64", List(0x29, 0xf8))
    }

    @Test
    def fpb32tor64(): Unit = {
        indirect("fpb32tor64", List(0x29, 0xfa))
    }

    @Test
    def fptesterr(): Unit = {
        indirect("fptesterr", List(0x29, 0xfc))
    }

    @Test
    def fprtoi32(): Unit = {
        indirect("fprtoi32", List(0x29, 0xfd))
    }

    @Test
    def fpstnli32(): Unit = {
        indirect("fpstnli32", List(0x29, 0xfe))
    }

    @Test
    def fpldzerosn(): Unit = {
        indirect("fpldzerosn", List(0x29, 0xff))
    }

    @Test
    def fpldzerodb(): Unit = {
        indirect("fpldzerodb", List(0x2a, 0xf0))
    }

    @Test
    def fpint(): Unit = {
        indirect("fpint", List(0x2a, 0xf1))
    }

    @Test
    def fpdup(): Unit = {
        indirect("fpdup", List(0x2a, 0xf3))
    }

    @Test
    def fprev(): Unit = {
        indirect("fprev", List(0x2a, 0xf4))
    }

    @Test
    def fpldnladddb(): Unit = {
        indirect("fpldnladddb", List(0x2a, 0xf6))
    }

    @Test
    def fpldnlmuldb(): Unit = {
        indirect("fpldnlmuldb", List(0x2a, 0xf8))
    }

    @Test
    def fpldnladdsn(): Unit = {
        indirect("fpldnladdsn", List(0x2a, 0xfa))
    }

    @Test
    def fpentry(): Unit = {
        indirect("fpentry", List(0x2a, 0xfb))
    }

    @Test
    def fpldnlmulsn(): Unit = {
        indirect("fpldnlmulsn", List(0x2a, 0xfc))
    }

    @Test
    def fpusqrtfirst(): Unit = {
        indirect("fpusqrtfirst", List(0x41, 0x2a, 0xfb))
    }

    @Test
    def fpusqrtstep(): Unit = {
        indirect("fpusqrtstep", List(0x42, 0x2a, 0xfb))
    }

    @Test
    def fpusqrtlast(): Unit = {
        indirect("fpusqrtlast", List(0x43, 0x2a, 0xfb))
    }

    @Test
    def fpurp(): Unit = {
        indirect("fpurp", List(0x44, 0x2a, 0xfb))
    }

    @Test
    def fpurm(): Unit = {
        indirect("fpurm", List(0x45, 0x2a, 0xfb))
    }

    @Test
    def fpurz(): Unit = {
        indirect("fpurz", List(0x46, 0x2a, 0xfb))
    }

    @Test
    def fpur32tor64(): Unit = {
        indirect("fpur32tor64", List(0x47, 0x2a, 0xfb))
    }

    @Test
    def fpur64tor32(): Unit = {
        indirect("fpur64tor32", List(0x48, 0x2a, 0xfb))
    }

    @Test
    def fpuexpdec32(): Unit = {
        indirect("fpuexpdec32", List(0x49, 0x2a, 0xfb))
    }

    @Test
    def fpuexpinc32(): Unit = {
        indirect("fpuexpinc32", List(0x4a, 0x2a, 0xfb))
    }

    @Test
    def fpuabs(): Unit = {
        indirect("fpuabs", List(0x4b, 0x2a, 0xfb))
    }

    @Test
    def fpunoround(): Unit = {
        indirect("fpunoround", List(0x4d, 0x2a, 0xfb))
    }

    @Test
    def fpuchki32(): Unit = {
        indirect("fpuchki32", List(0x4e, 0x2a, 0xfb))
    }

    @Test
    def fpuchki64(): Unit = {
        indirect("fpuchki64", List(0x4f, 0x2a, 0xfb))
    }

    @Test
    def fpudivby2(): Unit = {
        indirect("fpudivby2", List(0x21, 0x41, 0x2a, 0xfb))
    }

    @Test
    def fpumulby2(): Unit = {
        indirect("fpumulby2", List(0x21, 0x42, 0x2a, 0xfb))
    }

    @Test
    def fpurn(): Unit = {
        indirect("fpurn", List(0x22, 0x42, 0x2a, 0xfb))
    }

    @Test
    def fpuseterr(): Unit = {
        indirect("fpuseterr", List(0x22, 0x43, 0x2a, 0xfb))
    }

    @Test
    def fpuclrerr(): Unit = {
        indirect("fpuclrerr", List(0x29, 0x4c, 0x2a, 0xfb))
    }

    // T801 instructions
    @Test
    def start(): Unit = {
        indirect("start", List(0x21, 0x2f, 0xff))
    }

    @Test
    def testhardchan(): Unit = {
        indirect("testhardchan", List(0x22, 0xfd))
    }

    @Test
    def testldd(): Unit = {
        indirect("testldd", List(0x22, 0xf5))
    }

    @Test
    def teststd(): Unit = {
        indirect("teststd", List(0x22, 0xf8))
    }

    @Test
    def testlde(): Unit = {
        indirect("testlde", List(0x22, 0xf4))
    }

    @Test
    def testste(): Unit = {
        indirect("testste", List(0x22, 0xf7))
    }

    @Test
    def testlds(): Unit = {
        indirect("testlds", List(0x22, 0xf3))
    }

    @Test
    def teststs(): Unit = {
        indirect("teststs", List(0x22, 0xf6))
    }

    // T805 Instructions
    @Test
    def break(): Unit = {
        indirect("break", List(0x2b, 0xf1))
    }

    @Test
    def clrj0break(): Unit = {
        indirect("clrj0break", List(0x2b, 0xf2))
    }

    @Test
    def setj0break(): Unit = {
        indirect("setj0break", List(0x2b, 0xf3))
    }

    @Test
    def testj0break(): Unit = {
        indirect("testj0break", List(0x2b, 0xf4))
    }

    @Test
    def timerdisableh(): Unit = {
        indirect("timerdisableh", List(0x27, 0xfa))
    }

    @Test
    def timerdisablel(): Unit = {
        indirect("timerdisablel", List(0x27, 0xfb))
    }

    @Test
    def timerenableh(): Unit = {
        indirect("timerenableh", List(0x27, 0xfc))
    }

    @Test
    def timerenablel(): Unit = {
        indirect("timerenablel", List(0x27, 0xfd))
    }

    @Test
    def ldmemstartval(): Unit = {
        indirect("ldmemstartval", List(0x27, 0xfe))
    }

    @Test
    def pop(): Unit = {
        indirect("pop", List(0x27, 0xf9))
    }

    @Test
    def lddevid(): Unit = {
        indirect("lddevid", List(0x21, 0x27, 0xfc))
    }

    // Nonstandard emulator
    @Test
    def togglemonitor(): Unit = {
        indirect("togglemonitor", List(0x2c, 0xf0))
    }

    @Test
    def toggledisasm(): Unit = {
        indirect("toggledisasm", List(0x2c, 0xf1))
    }

    @Test
    def terminate(): Unit = {
        indirect("terminate", List(0x2c, 0xf2))
    }

    @Test
    def marker(): Unit = {
        indirect("marker", List(0x2c, 0xf3))
    }

    @Test
    def emuquery(): Unit = {
        indirect("emuquery", List(0x2c, 0xf4))
    }
}
