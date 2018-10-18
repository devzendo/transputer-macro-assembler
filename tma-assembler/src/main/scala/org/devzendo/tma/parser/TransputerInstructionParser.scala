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
import org.devzendo.tma.ast._

trait TransputerInstructionParser extends ExpressionParser with DiagnosableParser {

    @throws(classOf[AssemblyParserException])
    def parse(line: String): Statement = {
        val (sanitizedInput: String, parserOutput: ParseResult[Statement]) = parseStatement(line)
        parserOutput match {
            case Success(statement, _) =>
                if (debugParser) {
                    logger.debug("returning statement " + statement)
                }
                statement

            case NoSuccess(_, _) =>
                logger.debug(s"$lineNumber: $line") // mostly a useless, hard to understand error...
                throw new AssemblyParserException(lineNumber, "Unknown statement '" + sanitizedInput + "'")
        }
    }

    def parseStatement(line: String): (String, ParseResult[Statement]) = {
        val sanitizedInput = (if (line == null) "" else line).trim()
        val parseResult = parseAll(transputerInstruction, sanitizedInput)
        (sanitizedInput, parseResult)
    }

    def transputerInstruction: Parser[Statement] = indirectInstruction | directInstruction

    private def directInstruction: Parser[DirectInstruction] = (
      directOpcode ~ expression
      ) ^^ {
        case opcode ~ expression =>
            if (debugParser) logger.debug("in directInstruction, opcode: " + opcode + " expr:" + expression)
            DirectInstruction(opcode._1, opcode._2, expression)
    }

    private val OP_J = 0x00
    private val OP_LDLP = 0x10
    private val OP_PFIX = 0x20
    private val OP_LDNL = 0x30
    private val OP_LDC = 0x40
    private val OP_LDNLP = 0x50
    private val OP_NFIX = 0x60
    private val OP_LDL = 0x70
    private val OP_ADC = 0x80
    private val OP_CALL = 0x90
    private val OP_CJ = 0xa0
    private val OP_AJW = 0xb0
    private val OP_EQC = 0xc0
    private val OP_STL = 0xd0
    private val OP_STNL = 0xe0
    private val OP_OPR = 0xf0

    // in numeric order, but need to move ldnlp before ldnl so longest length parses first
    private def directOpcode: Parser[(String, Int)] = j | ldlp | pfix | ldnlp | ldnl | ldc | nfix | ldl | adc | call | cj | ajw | eqc | stl | stnl | opr

    private def j: Parser[(String, Int)]     = """(?i)J""".r     ^^ ( x => (x.toUpperCase, OP_J) )
    private def ldlp: Parser[(String, Int)]  = """(?i)LDLP""".r  ^^ ( x => (x.toUpperCase, OP_LDLP) )
    private def pfix: Parser[(String, Int)]  = """(?i)PFIX""".r  ^^ ( x => (x.toUpperCase, OP_PFIX) )
    private def ldnl: Parser[(String, Int)]  = """(?i)LDNL""".r  ^^ ( x => (x.toUpperCase, OP_LDNL) )
    private def ldc: Parser[(String, Int)]   = """(?i)LDC""".r   ^^ ( x => (x.toUpperCase, OP_LDC) )
    private def ldnlp: Parser[(String, Int)] = """(?i)LDNLP""".r ^^ ( x => (x.toUpperCase, OP_LDNLP) )
    private def nfix: Parser[(String, Int)]  = """(?i)NFIX""".r  ^^ ( x => (x.toUpperCase, OP_NFIX) )
    private def ldl: Parser[(String, Int)]   = """(?i)LDL""".r   ^^ ( x => (x.toUpperCase, OP_LDL) )
    private def adc: Parser[(String, Int)]   = """(?i)ADC""".r   ^^ ( x => (x.toUpperCase, OP_ADC) )
    private def call: Parser[(String, Int)]  = """(?i)CALL""".r  ^^ ( x => (x.toUpperCase, OP_CALL) )
    private def cj: Parser[(String, Int)]    = """(?i)CJ""".r    ^^ ( x => (x.toUpperCase, OP_CJ) )
    private def ajw: Parser[(String, Int)]   = """(?i)AJW""".r   ^^ ( x => (x.toUpperCase, OP_AJW) )
    private def eqc: Parser[(String, Int)]   = """(?i)EQC""".r   ^^ ( x => (x.toUpperCase, OP_EQC) )
    private def stl: Parser[(String, Int)]   = """(?i)STL""".r   ^^ ( x => (x.toUpperCase, OP_STL) )
    private def stnl: Parser[(String, Int)]  = """(?i)STNL""".r  ^^ ( x => (x.toUpperCase, OP_STNL) )
    private def opr: Parser[(String, Int)]   = """(?i)OPR""".r   ^^ ( x => (x.toUpperCase, OP_OPR) )



    private def indirectInstruction: Parser[IndirectInstruction] =
        indirectOpcode ^^ {
            opcode =>
                if (debugParser) logger.debug("in indirectInstruction, opcode: " + opcode)
                IndirectInstruction(opcode._1, opcode._2)
        }

    private def indirectOpcode: Parser[(String, List[Int])] =
        rev | lb | bsub | endp | diff | add | gcall | in | prod | gt | sub | startp | outbyte | outword | out |
        seterr | resetch | csub0 | stopp | ladd | stlb | sthf | norm | ldiv | ldpi | stlf | xdble | ldpri | rem | ret |
        lend | ldtimer | testerr | testpranal | tin | div | dist | disc | diss | lmul | _not | xor | bcnt | lshr | lshl |
        lsum | lsub | runp | xword | sb | gajw | savel | saveh | wcnt | _shr | _shl | mint |
        altend | _and | enbt | enbc | enbs | _or | csngl | ccnt1 | ldiff | sthb | taltwt | altwt | talt | alt | sum |
        mul | sttimer | stoperr | cword | clrhalterr | sethalterr | testhalterr | dup | move2dinit |
        move2dall | move2dnonzero | move2dzero | move | unpacksn | postnormsn | roundsn | ldinf | fmul | cflerr |
        crcword | crcbyte | bitcnt | bitrevword | bitrevnbits | wsubdb | wsub | fpldnldbi | fpchkerr | fpstnldb |
        fpldnlsni | fpadd | fpstnlsn | fpsub | fpldnldb | fpmul | fpdiv | fpldnlsn | fpremfirst | fpremstep |
        fpnan | fpordered | fpnotfinite | fpgt | fpeq | fpi32tor32 | fpi32tor64 | fpb32tor64 | fptesterr |
        fprtoi32 | fpstnli32 | fpldzerosn | fpldzerodb | fpint | fpdup | fprev | fpldnladddb | fpldnlmuldb |
        fpldnladdsn | fpentry | fpldnlmulsn | fpusqrtfirst | fpusqrtstep | fpusqrtlast | fpurp | fpurm |
        fpurz | fpur32tor64 | fpur64tor32 | fpuexpdec32 | fpuexpinc32 | fpuabs | fpunoround | fpuchki32 |
        fpuchki64 | fpudivby2 | fpumulby2 | fpurn | fpuseterr | fpuclrerr |
        // T801
        start | testhardchan | testldd | teststd | testlde | testste | testlds | teststs |
        // T805
        break | clrj0break | setj0break | testj0break | timerdisablel | timerdisableh | timerenableh | timerenablel |
        ldmemstartval | pop | lddevid

    // Opcode to direct instructions. As per CWG, p 117
    private def direct(opcode: Int): List[Int] = List(OP_OPR | (opcode & 0x0f)) // OPR(opcode)

    private def indirect2(opcode: Int): List[Int] = {
        List(OP_PFIX | ((opcode & 0xf0) >> 4),  // PFIX(opcode left-nybble)
             OP_OPR  | (opcode & 0x0f))         // OPR(opcode right-nybble)
    }

    private def indirect3(opcode: Int): List[Int] = {
        List(OP_PFIX | ((opcode & 0xf00) >> 8),  // PFIX(opcode nybble 2)
             OP_PFIX | ((opcode & 0x0f0) >> 4),  // PFIX(opcode nybble 1)
             OP_OPR  |  (opcode & 0x0f))         // OPR(opcode nybble 0)
    }

    private def indirectfpentry(opcode: Int): List[Int] = {
        val areg = collection.mutable.ArrayBuffer[Int]()
        if (opcode > 0x0f) {
            // Two nybbles into Areg
            areg += (OP_PFIX | ((opcode & 0xf0) >> 4)) // PFIX(opcode left-nybble)
        }
        // Single nybble into Areg
        areg += (OP_LDC | (opcode & 0x0f)) // LDC(opcode right-nybble)

        areg ++= indirect2(0xAB) // FPENTRY
        areg.toList
    }

    private def rev: Parser[(String, List[Int])]       = """(?i)REV""".r       ^^ ( x => (x.toUpperCase, direct(0x00)) )
    private def lb: Parser[(String, List[Int])]        = """(?i)LB""".r        ^^ ( x => (x.toUpperCase, direct(0x01)) )
    private def bsub: Parser[(String, List[Int])]      = """(?i)BSUB""".r      ^^ ( x => (x.toUpperCase, direct(0x02)) )
    private def endp: Parser[(String, List[Int])]      = """(?i)ENDP""".r      ^^ ( x => (x.toUpperCase, direct(0x03)) )
    private def diff: Parser[(String, List[Int])]      = """(?i)DIFF""".r      ^^ ( x => (x.toUpperCase, direct(0x04)) )
    private def add: Parser[(String, List[Int])]       = """(?i)ADD""".r       ^^ ( x => (x.toUpperCase, direct(0x05)) )
    private def gcall: Parser[(String, List[Int])]     = """(?i)GCALL""".r     ^^ ( x => (x.toUpperCase, direct(0x06)) )
    private def in: Parser[(String, List[Int])]        = """(?i)IN""".r        ^^ ( x => (x.toUpperCase, direct(0x07)) )
    private def prod: Parser[(String, List[Int])]      = """(?i)PROD""".r      ^^ ( x => (x.toUpperCase, direct(0x08)) )
    private def gt: Parser[(String, List[Int])]        = """(?i)GT""".r        ^^ ( x => (x.toUpperCase, direct(0x09)) )
    private def wsub: Parser[(String, List[Int])]      = """(?i)WSUB""".r      ^^ ( x => (x.toUpperCase, direct(0x0a)) )
    private def out: Parser[(String, List[Int])]       = """(?i)OUT""".r       ^^ ( x => (x.toUpperCase, direct(0x0b)) )
    private def sub: Parser[(String, List[Int])]       = """(?i)SUB""".r       ^^ ( x => (x.toUpperCase, direct(0x0c)) )
    private def startp: Parser[(String, List[Int])]    = """(?i)STARTP""".r    ^^ ( x => (x.toUpperCase, direct(0x0d)) )
    private def outbyte: Parser[(String, List[Int])]   = """(?i)OUTBYTE""".r   ^^ ( x => (x.toUpperCase, direct(0x0e)) )
    private def outword: Parser[(String, List[Int])]   = """(?i)OUTWORD""".r   ^^ ( x => (x.toUpperCase, direct(0x0f)) )

    private def seterr: Parser[(String, List[Int])]    = """(?i)SETERR""".r    ^^ ( x => (x.toUpperCase, indirect2(0x10)) )
    private def resetch: Parser[(String, List[Int])]   = """(?i)RESETCH""".r   ^^ ( x => (x.toUpperCase, indirect2(0x12)) )
    private def csub0: Parser[(String, List[Int])]     = """(?i)CSUB0""".r     ^^ ( x => (x.toUpperCase, indirect2(0x13)) )
    private def stopp: Parser[(String, List[Int])]     = """(?i)STOPP""".r     ^^ ( x => (x.toUpperCase, indirect2(0x15)) )
    private def ladd: Parser[(String, List[Int])]      = """(?i)LADD""".r      ^^ ( x => (x.toUpperCase, indirect2(0x16)) )
    private def stlb: Parser[(String, List[Int])]      = """(?i)STLB""".r      ^^ ( x => (x.toUpperCase, indirect2(0x17)) )
    private def sthf: Parser[(String, List[Int])]      = """(?i)STHF""".r      ^^ ( x => (x.toUpperCase, indirect2(0x18)) )
    private def norm: Parser[(String, List[Int])]      = """(?i)NORM""".r      ^^ ( x => (x.toUpperCase, indirect2(0x19)) )
    private def ldiv: Parser[(String, List[Int])]      = """(?i)LDIV""".r      ^^ ( x => (x.toUpperCase, indirect2(0x1a)) )
    private def ldpi: Parser[(String, List[Int])]      = """(?i)LDPI""".r      ^^ ( x => (x.toUpperCase, indirect2(0x1b)) )
    private def stlf: Parser[(String, List[Int])]      = """(?i)STLF""".r      ^^ ( x => (x.toUpperCase, indirect2(0x1c)) )
    private def xdble: Parser[(String, List[Int])]     = """(?i)XDBLE""".r     ^^ ( x => (x.toUpperCase, indirect2(0x1d)) )
    private def ldpri: Parser[(String, List[Int])]     = """(?i)LDPRI""".r     ^^ ( x => (x.toUpperCase, indirect2(0x1e)) )
    private def rem: Parser[(String, List[Int])]       = """(?i)REM""".r       ^^ ( x => (x.toUpperCase, indirect2(0x1f)) )
    private def ret: Parser[(String, List[Int])]       = """(?i)RET""".r       ^^ ( x => (x.toUpperCase, indirect2(0x20)) )
    private def lend: Parser[(String, List[Int])]      = """(?i)LEND""".r      ^^ ( x => (x.toUpperCase, indirect2(0x21)) )
    private def ldtimer: Parser[(String, List[Int])]   = """(?i)LDTIMER""".r   ^^ ( x => (x.toUpperCase, indirect2(0x22)) )
    private def testerr: Parser[(String, List[Int])]   = """(?i)TESTERR""".r   ^^ ( x => (x.toUpperCase, indirect2(0x29)) )
    private def testpranal: Parser[(String, List[Int])] = """(?i)TESTPRANAL""".r ^^ ( x => (x.toUpperCase, indirect2(0x2a)) )
    private def tin: Parser[(String, List[Int])]       = """(?i)TIN""".r       ^^ ( x => (x.toUpperCase, indirect2(0x2b)) )
    private def div: Parser[(String, List[Int])]       = """(?i)DIV""".r       ^^ ( x => (x.toUpperCase, indirect2(0x2c)) )
    private def dist: Parser[(String, List[Int])]      = """(?i)DIST""".r      ^^ ( x => (x.toUpperCase, indirect2(0x2e)) )
    private def disc: Parser[(String, List[Int])]      = """(?i)DISC""".r      ^^ ( x => (x.toUpperCase, indirect2(0x2f)) )
    private def diss: Parser[(String, List[Int])]      = """(?i)DISS""".r      ^^ ( x => (x.toUpperCase, indirect2(0x30)) )
    private def lmul: Parser[(String, List[Int])]      = """(?i)LMUL""".r      ^^ ( x => (x.toUpperCase, indirect2(0x31)) )
    private def _not: Parser[(String, List[Int])]      = """(?i)NOT""".r       ^^ ( x => (x.toUpperCase, indirect2(0x32)) )
    private def xor: Parser[(String, List[Int])]       = """(?i)XOR""".r       ^^ ( x => (x.toUpperCase, indirect2(0x33)) )
    private def bcnt: Parser[(String, List[Int])]      = """(?i)BCNT""".r      ^^ ( x => (x.toUpperCase, indirect2(0x34)) )
    private def lshr: Parser[(String, List[Int])]      = """(?i)LSHR""".r      ^^ ( x => (x.toUpperCase, indirect2(0x35)) )
    private def lshl: Parser[(String, List[Int])]      = """(?i)LSHL""".r      ^^ ( x => (x.toUpperCase, indirect2(0x36)) )
    private def lsum: Parser[(String, List[Int])]      = """(?i)LSUM""".r      ^^ ( x => (x.toUpperCase, indirect2(0x37)) )
    private def lsub: Parser[(String, List[Int])]      = """(?i)LSUB""".r      ^^ ( x => (x.toUpperCase, indirect2(0x38)) )
    private def runp: Parser[(String, List[Int])]      = """(?i)RUNP""".r      ^^ ( x => (x.toUpperCase, indirect2(0x39)) )
    private def xword: Parser[(String, List[Int])]     = """(?i)XWORD""".r     ^^ ( x => (x.toUpperCase, indirect2(0x3a)) )
    private def sb: Parser[(String, List[Int])]        = """(?i)SB""".r        ^^ ( x => (x.toUpperCase, indirect2(0x3b)) )
    private def gajw: Parser[(String, List[Int])]      = """(?i)GAJW""".r      ^^ ( x => (x.toUpperCase, indirect2(0x3c)) )
    private def savel: Parser[(String, List[Int])]     = """(?i)SAVEL""".r     ^^ ( x => (x.toUpperCase, indirect2(0x3d)) )
    private def saveh: Parser[(String, List[Int])]     = """(?i)SAVEH""".r     ^^ ( x => (x.toUpperCase, indirect2(0x3e)) )
    private def wcnt: Parser[(String, List[Int])]      = """(?i)WCNT""".r      ^^ ( x => (x.toUpperCase, indirect2(0x3f)) )
    private def _shr: Parser[(String, List[Int])]      = """(?i)SHR""".r       ^^ ( x => (x.toUpperCase, indirect2(0x40)) )
    private def _shl: Parser[(String, List[Int])]      = """(?i)SHL""".r       ^^ ( x => (x.toUpperCase, indirect2(0x41)) )
    private def mint: Parser[(String, List[Int])]      = """(?i)MINT""".r      ^^ ( x => (x.toUpperCase, indirect2(0x42)) )
    private def alt: Parser[(String, List[Int])]       = """(?i)ALT""".r       ^^ ( x => (x.toUpperCase, indirect2(0x43)) )
    private def altwt: Parser[(String, List[Int])]     = """(?i)ALTWT""".r     ^^ ( x => (x.toUpperCase, indirect2(0x44)) )
    private def altend: Parser[(String, List[Int])]    = """(?i)ALTEND""".r    ^^ ( x => (x.toUpperCase, indirect2(0x45)) )
    private def _and: Parser[(String, List[Int])]      = """(?i)AND""".r       ^^ ( x => (x.toUpperCase, indirect2(0x46)) )
    private def enbt: Parser[(String, List[Int])]      = """(?i)ENBT""".r      ^^ ( x => (x.toUpperCase, indirect2(0x47)) )
    private def enbc: Parser[(String, List[Int])]      = """(?i)ENBC""".r      ^^ ( x => (x.toUpperCase, indirect2(0x48)) )
    private def enbs: Parser[(String, List[Int])]      = """(?i)ENBS""".r      ^^ ( x => (x.toUpperCase, indirect2(0x49)) )
    private def move: Parser[(String, List[Int])]      = """(?i)MOVE""".r      ^^ ( x => (x.toUpperCase, indirect2(0x4a)) )
    private def _or: Parser[(String, List[Int])]       = """(?i)OR""".r        ^^ ( x => (x.toUpperCase, indirect2(0x4b)) )
    private def csngl: Parser[(String, List[Int])]     = """(?i)CSNGL""".r     ^^ ( x => (x.toUpperCase, indirect2(0x4c)) )
    private def ccnt1: Parser[(String, List[Int])]     = """(?i)CCNT1""".r     ^^ ( x => (x.toUpperCase, indirect2(0x4d)) )
    private def talt: Parser[(String, List[Int])]      = """(?i)TALT""".r      ^^ ( x => (x.toUpperCase, indirect2(0x4e)) )
    private def ldiff: Parser[(String, List[Int])]     = """(?i)LDIFF""".r     ^^ ( x => (x.toUpperCase, indirect2(0x4f)) )
    private def sthb: Parser[(String, List[Int])]      = """(?i)STHB""".r      ^^ ( x => (x.toUpperCase, indirect2(0x50)) )
    private def taltwt: Parser[(String, List[Int])]    = """(?i)TALTWT""".r    ^^ ( x => (x.toUpperCase, indirect2(0x51)) )
    private def sum: Parser[(String, List[Int])]       = """(?i)SUM""".r       ^^ ( x => (x.toUpperCase, indirect2(0x52)) )
    private def mul: Parser[(String, List[Int])]       = """(?i)MUL""".r       ^^ ( x => (x.toUpperCase, indirect2(0x53)) )
    private def sttimer: Parser[(String, List[Int])]   = """(?i)STTIMER""".r   ^^ ( x => (x.toUpperCase, indirect2(0x54)) )
    private def stoperr: Parser[(String, List[Int])]   = """(?i)STOPERR""".r   ^^ ( x => (x.toUpperCase, indirect2(0x55)) )
    private def cword: Parser[(String, List[Int])]     = """(?i)CWORD""".r     ^^ ( x => (x.toUpperCase, indirect2(0x56)) )
    private def clrhalterr: Parser[(String, List[Int])] = """(?i)CLRHALTERR""".r ^^ ( x => (x.toUpperCase, indirect2(0x57)) )
    private def sethalterr: Parser[(String, List[Int])] = """(?i)SETHALTERR""".r ^^ ( x => (x.toUpperCase, indirect2(0x58)) )
    private def testhalterr: Parser[(String, List[Int])] = """(?i)TESTHALTERR""".r ^^ ( x => (x.toUpperCase, indirect2(0x59)) )
    private def dup: Parser[(String, List[Int])]       = """(?i)DUP""".r       ^^ ( x => (x.toUpperCase, indirect2(0x5a)) )
    private def move2dinit: Parser[(String, List[Int])] = """(?i)MOVE2DINIT""".r ^^ ( x => (x.toUpperCase, indirect2(0x5b)) )
    private def move2dall: Parser[(String, List[Int])] = """(?i)MOVE2DALL""".r ^^ ( x => (x.toUpperCase, indirect2(0x5c)) )
    private def move2dnonzero: Parser[(String, List[Int])] = """(?i)MOVE2DNONZERO""".r ^^ ( x => (x.toUpperCase, indirect2(0x5d)) )
    private def move2dzero: Parser[(String, List[Int])] = """(?i)MOVE2DZERO""".r ^^ ( x => (x.toUpperCase, indirect2(0x5e)) )
    private def unpacksn: Parser[(String, List[Int])]  = """(?i)UNPACKSN""".r  ^^ ( x => (x.toUpperCase, indirect2(0x63)) )
    private def postnormsn: Parser[(String, List[Int])] = """(?i)POSTNORMSN""".r ^^ ( x => (x.toUpperCase, indirect2(0x6c)) )
    private def roundsn: Parser[(String, List[Int])]   = """(?i)ROUNDSN""".r   ^^ ( x => (x.toUpperCase, indirect2(0x6d)) )
    private def ldinf: Parser[(String, List[Int])]     = """(?i)LDINF""".r     ^^ ( x => (x.toUpperCase, indirect2(0x71)) )
    private def fmul: Parser[(String, List[Int])]      = """(?i)FMUL""".r      ^^ ( x => (x.toUpperCase, indirect2(0x72)) )
    private def cflerr: Parser[(String, List[Int])]    = """(?i)CFLERR""".r    ^^ ( x => (x.toUpperCase, indirect2(0x73)) )
    private def crcword: Parser[(String, List[Int])]   = """(?i)CRCWORD""".r   ^^ ( x => (x.toUpperCase, indirect2(0x74)) )
    private def crcbyte: Parser[(String, List[Int])]   = """(?i)CRCBYTE""".r   ^^ ( x => (x.toUpperCase, indirect2(0x75)) )
    private def bitcnt: Parser[(String, List[Int])]    = """(?i)BITCNT""".r    ^^ ( x => (x.toUpperCase, indirect2(0x76)) )
    private def bitrevword: Parser[(String, List[Int])] = """(?i)BITREVWORD""".r ^^ ( x => (x.toUpperCase, indirect2(0x77)) )
    private def bitrevnbits: Parser[(String, List[Int])] = """(?i)BITREVNBITS""".r ^^ ( x => (x.toUpperCase, indirect2(0x78)) )
    private def wsubdb: Parser[(String, List[Int])]    = """(?i)WSUBDB""".r    ^^ ( x => (x.toUpperCase, indirect2(0x81)) )
    private def fpldnldbi: Parser[(String, List[Int])] = """(?i)FPLDNLDBI""".r ^^ ( x => (x.toUpperCase, indirect2(0x82)) )
    private def fpchkerr: Parser[(String, List[Int])]  = """(?i)FPCHKERR""".r  ^^ ( x => (x.toUpperCase, indirect2(0x83)) )
    private def fpstnldb: Parser[(String, List[Int])]  = """(?i)FPSTNLDB""".r  ^^ ( x => (x.toUpperCase, indirect2(0x84)) )
    private def fpldnlsni: Parser[(String, List[Int])] = """(?i)FPLDNLSNI""".r ^^ ( x => (x.toUpperCase, indirect2(0x86)) )
    private def fpadd: Parser[(String, List[Int])]     = """(?i)FPADD""".r     ^^ ( x => (x.toUpperCase, indirect2(0x87)) )
    private def fpstnlsn: Parser[(String, List[Int])]  = """(?i)FPSTNLSN""".r  ^^ ( x => (x.toUpperCase, indirect2(0x88)) )
    private def fpsub: Parser[(String, List[Int])]     = """(?i)FPSUB""".r     ^^ ( x => (x.toUpperCase, indirect2(0x89)) )
    private def fpldnldb: Parser[(String, List[Int])]  = """(?i)FPLDNLDB""".r  ^^ ( x => (x.toUpperCase, indirect2(0x8a)) )
    private def fpmul: Parser[(String, List[Int])]     = """(?i)FPMUL""".r     ^^ ( x => (x.toUpperCase, indirect2(0x8b)) )
    private def fpdiv: Parser[(String, List[Int])]     = """(?i)FPDIV""".r     ^^ ( x => (x.toUpperCase, indirect2(0x8c)) )
    private def fpldnlsn: Parser[(String, List[Int])]  = """(?i)FPLDNLSN""".r  ^^ ( x => (x.toUpperCase, indirect2(0x8e)) )
    private def fpremfirst: Parser[(String, List[Int])] = """(?i)FPREMFIRST""".r ^^ ( x => (x.toUpperCase, indirect2(0x8f)) )
    private def fpremstep: Parser[(String, List[Int])] = """(?i)FPREMSTEP""".r ^^ ( x => (x.toUpperCase, indirect2(0x90)) )
    private def fpnan: Parser[(String, List[Int])]     = """(?i)FPNAN""".r     ^^ ( x => (x.toUpperCase, indirect2(0x91)) )
    private def fpordered: Parser[(String, List[Int])] = """(?i)FPORDERED""".r ^^ ( x => (x.toUpperCase, indirect2(0x92)) )
    private def fpnotfinite: Parser[(String, List[Int])] = """(?i)FPNOTFINITE""".r ^^ ( x => (x.toUpperCase, indirect2(0x93)) )
    private def fpgt: Parser[(String, List[Int])]      = """(?i)FPGT""".r      ^^ ( x => (x.toUpperCase, indirect2(0x94)) )
    private def fpeq: Parser[(String, List[Int])]      = """(?i)FPEQ""".r      ^^ ( x => (x.toUpperCase, indirect2(0x95)) )
    private def fpi32tor32: Parser[(String, List[Int])] = """(?i)FPI32TOR32""".r ^^ ( x => (x.toUpperCase, indirect2(0x96)) )
    private def fpi32tor64: Parser[(String, List[Int])] = """(?i)FPI32TOR64""".r ^^ ( x => (x.toUpperCase, indirect2(0x98)) )
    private def fpb32tor64: Parser[(String, List[Int])] = """(?i)FPB32TOR64""".r ^^ ( x => (x.toUpperCase, indirect2(0x9a)) )
    private def fptesterr: Parser[(String, List[Int])] = """(?i)FPTESTERR""".r   ^^ ( x => (x.toUpperCase, indirect2(0x9c)) )
    private def fprtoi32: Parser[(String, List[Int])]  = """(?i)FPRTOI32""".r     ^^ ( x => (x.toUpperCase, indirect2(0x9d)) )
    private def fpstnli32: Parser[(String, List[Int])] = """(?i)FPSTNLI32""".r   ^^ ( x => (x.toUpperCase, indirect2(0x9e)) )
    private def fpldzerosn: Parser[(String, List[Int])] = """(?i)FPLDZEROSN""".r ^^ ( x => (x.toUpperCase, indirect2(0x9f)) )
    private def fpldzerodb: Parser[(String, List[Int])] = """(?i)FPLDZERODB""".r ^^ ( x => (x.toUpperCase, indirect2(0xa0)) )
    private def fpint: Parser[(String, List[Int])]     = """(?i)FPINT""".r       ^^ ( x => (x.toUpperCase, indirect2(0xa1)) )
    private def fpdup: Parser[(String, List[Int])]     = """(?i)FPDUP""".r       ^^ ( x => (x.toUpperCase, indirect2(0xa3)) )
    private def fprev: Parser[(String, List[Int])]     = """(?i)FPREV""".r       ^^ ( x => (x.toUpperCase, indirect2(0xa4)) )
    private def fpldnladddb: Parser[(String, List[Int])] = """(?i)FPLDNLADDDB""".r ^^ ( x => (x.toUpperCase, indirect2(0xa6)) )
    private def fpldnlmuldb: Parser[(String, List[Int])] = """(?i)FPLDNLMULDB""".r ^^ ( x => (x.toUpperCase, indirect2(0xa8)) )
    private def fpldnladdsn: Parser[(String, List[Int])] = """(?i)FPLDNLADDSN""".r ^^ ( x => (x.toUpperCase, indirect2(0xaa)) )
    private def fpentry: Parser[(String, List[Int])]   = """(?i)FPENTRY""".r     ^^ ( x => (x.toUpperCase, indirect2(0xab)) )
    private def fpldnlmulsn: Parser[(String, List[Int])] = """(?i)FPLDNLMULSN""".r ^^ ( x => (x.toUpperCase, indirect2(0xac)) )

    private def fpusqrtfirst: Parser[(String, List[Int])] = """(?i)FPUSQRTFIRST""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x01)) )
    private def fpusqrtstep: Parser[(String, List[Int])] = """(?i)FPUSQRTSTEP""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x02)) )
    private def fpusqrtlast: Parser[(String, List[Int])] = """(?i)FPUSQRTLAST""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x03)) )
    private def fpurp: Parser[(String, List[Int])]     = """(?i)FPURP""".r     ^^ ( x => (x.toUpperCase, indirectfpentry(0x04)) )
    private def fpurm: Parser[(String, List[Int])]     = """(?i)FPURM""".r     ^^ ( x => (x.toUpperCase, indirectfpentry(0x05)) )
    private def fpurz: Parser[(String, List[Int])]     = """(?i)FPURZ""".r     ^^ ( x => (x.toUpperCase, indirectfpentry(0x06)) )
    private def fpur32tor64: Parser[(String, List[Int])] = """(?i)FPUR32TOR64""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x07)) )
    private def fpur64tor32: Parser[(String, List[Int])] = """(?i)FPUR64TOR32""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x08)) )
    private def fpuexpdec32: Parser[(String, List[Int])] = """(?i)FPUEXPDEC32""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x09)) )
    private def fpuexpinc32: Parser[(String, List[Int])] = """(?i)FPUEXPINC32""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x0a)) )
    private def fpuabs: Parser[(String, List[Int])]    = """(?i)FPUABS""".r    ^^ ( x => (x.toUpperCase, indirectfpentry(0x0b)) )
    private def fpunoround: Parser[(String, List[Int])] = """(?i)FPUNOROUND""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x0d)) )
    private def fpuchki32: Parser[(String, List[Int])] = """(?i)FPUCHKI32""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x0e)) )
    private def fpuchki64: Parser[(String, List[Int])] = """(?i)FPUCHKI64""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x0f)) )
    private def fpudivby2: Parser[(String, List[Int])] = """(?i)FPUDIVBY2""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x11)) )
    private def fpumulby2: Parser[(String, List[Int])] = """(?i)FPUMULBY2""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x12)) )
    private def fpurn: Parser[(String, List[Int])]     = """(?i)FPURN""".r     ^^ ( x => (x.toUpperCase, indirectfpentry(0x22)) )
    private def fpuseterr: Parser[(String, List[Int])] = """(?i)FPUSETERR""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x23)) )
    private def fpuclrerr: Parser[(String, List[Int])] = """(?i)FPUCLRERR""".r ^^ ( x => (x.toUpperCase, indirectfpentry(0x9c)) )

    // T801 Instructions
    private def start: Parser[(String, List[Int])] = """(?i)START""".r ^^ ( x => (x.toUpperCase, indirect3(0x1ff)) )
    private def testhardchan: Parser[(String, List[Int])] = """(?i)TESTHARDCHAN""".r ^^ ( x => (x.toUpperCase, indirect2(0x2d)) )
    private def testldd: Parser[(String, List[Int])] = """(?i)TESTLDD""".r ^^ ( x => (x.toUpperCase, indirect2(0x25)) )
    private def teststd: Parser[(String, List[Int])] = """(?i)TESTSTD""".r ^^ ( x => (x.toUpperCase, indirect2(0x28)) )
    private def testlde: Parser[(String, List[Int])] = """(?i)TESTLDE""".r ^^ ( x => (x.toUpperCase, indirect2(0x24)) )
    private def testste: Parser[(String, List[Int])] = """(?i)TESTSTE""".r ^^ ( x => (x.toUpperCase, indirect2(0x27)) )
    private def testlds: Parser[(String, List[Int])] = """(?i)TESTLDS""".r ^^ ( x => (x.toUpperCase, indirect2(0x23)) )
    private def teststs: Parser[(String, List[Int])] = """(?i)TESTSTS""".r ^^ ( x => (x.toUpperCase, indirect2(0x26)) )

    // T805 Instructions
    private def break: Parser[(String, List[Int])] = """(?i)BREAK""".r ^^ ( x => (x.toUpperCase, indirect2(0xb1)) )
    private def clrj0break: Parser[(String, List[Int])] = """(?i)CLRJ0BREAK""".r ^^ ( x => (x.toUpperCase, indirect2(0xb2)) )
    private def setj0break: Parser[(String, List[Int])] = """(?i)SETJ0BREAK""".r ^^ ( x => (x.toUpperCase, indirect2(0xb3)) )
    private def testj0break: Parser[(String, List[Int])] = """(?i)TESTJ0BREAK""".r ^^ ( x => (x.toUpperCase, indirect2(0xb4)) )
    private def timerdisableh: Parser[(String, List[Int])] = """(?i)TIMERDISABLEH""".r ^^ ( x => (x.toUpperCase, indirect2(0x7a)) )
    private def timerdisablel: Parser[(String, List[Int])] = """(?i)TIMERDISABLEL""".r ^^ ( x => (x.toUpperCase, indirect2(0x7b)) )
    private def timerenableh: Parser[(String, List[Int])] = """(?i)TIMERENABLEH""".r ^^ ( x => (x.toUpperCase, indirect2(0x7c)) )
    private def timerenablel: Parser[(String, List[Int])] = """(?i)TIMERENABLEL""".r ^^ ( x => (x.toUpperCase, indirect2(0x7d)) )
    private def ldmemstartval: Parser[(String, List[Int])] = """(?i)LDMEMSTARTVAL""".r ^^ ( x => (x.toUpperCase, indirect2(0x7e)) )
    private def pop: Parser[(String, List[Int])] = """(?i)POP""".r ^^ ( x => (x.toUpperCase, indirect2(0x79)) )
    private def lddevid: Parser[(String, List[Int])] = """(?i)LDDEVID""".r ^^ ( x => (x.toUpperCase, indirect3(0x17c)) )
}
