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

trait T800InstructionParser extends ExpressionParser with DiagnosableParser {

    @throws(classOf[AssemblyParserException])
    def parse(line: String): Statement = {
        val (sanitizedInput: String, parserOutput: ParseResult[Statement]) = parseStatement(line)
        parserOutput match {
            case Success(statement, _) =>
                if (debugParser) {
                    logger.debug("returning statement " + statement)
                }
                statement

            case NoSuccess(x, _) =>
                logger.debug(s"$lineNumber: $line") // mostly a useless, hard to understand error...
                throw new AssemblyParserException(lineNumber, "Unknown statement '" + sanitizedInput + "'")
        }
    }

    def parseStatement(line: String) = {
        val sanitizedInput = (if (line == null) "" else line).trim()
        val parseResult = parseAll(t800Instruction, sanitizedInput)
        (sanitizedInput, parseResult)
    }

    def t800Instruction: Parser[Statement] = directInstruction | indirectInstruction

    private def directInstruction: Parser[DirectInstruction] = (
      directOpcode ~ expression
      ) ^^ {
        case opcode ~ expression =>
            if (debugParser) logger.debug("in directInstruction, opcode: " + opcode + " expr:" + expression)
            DirectInstruction(opcode._1, opcode._2, expression)
    }

    // in numeric order, but need to move ldnlp before ldnl so longest length parses first
    private def directOpcode: Parser[(String, Int)] = j | ldlp | pfix | ldnlp | ldnl | ldc | nfix | ldl | adc | call | cj | ajw | eqc | stl | stnl | opr

    private def j: Parser[(String, Int)]     = """(?i)J""".r     ^^ ( x => (x.toUpperCase, 0x00) )
    private def ldlp: Parser[(String, Int)]  = """(?i)LDLP""".r  ^^ ( x => (x.toUpperCase, 0x01) )
    private def pfix: Parser[(String, Int)]  = """(?i)PFIX""".r  ^^ ( x => (x.toUpperCase, 0x02) )
    private def ldnl: Parser[(String, Int)]  = """(?i)LDNL""".r  ^^ ( x => (x.toUpperCase, 0x03) )
    private def ldc: Parser[(String, Int)]   = """(?i)LDC""".r   ^^ ( x => (x.toUpperCase, 0x04) )
    private def ldnlp: Parser[(String, Int)] = """(?i)LDNLP""".r ^^ ( x => (x.toUpperCase, 0x05) )
    private def nfix: Parser[(String, Int)]  = """(?i)NFIX""".r  ^^ ( x => (x.toUpperCase, 0x06) )
    private def ldl: Parser[(String, Int)]   = """(?i)LDL""".r   ^^ ( x => (x.toUpperCase, 0x07) )
    private def adc: Parser[(String, Int)]   = """(?i)ADC""".r   ^^ ( x => (x.toUpperCase, 0x08) )
    private def call: Parser[(String, Int)]  = """(?i)CALL""".r  ^^ ( x => (x.toUpperCase, 0x09) )
    private def cj: Parser[(String, Int)]    = """(?i)CJ""".r    ^^ ( x => (x.toUpperCase, 0x0a) )
    private def ajw: Parser[(String, Int)]   = """(?i)AJW""".r   ^^ ( x => (x.toUpperCase, 0x0b) )
    private def eqc: Parser[(String, Int)]   = """(?i)EQC""".r   ^^ ( x => (x.toUpperCase, 0x0c) )
    private def stl: Parser[(String, Int)]   = """(?i)STL""".r   ^^ ( x => (x.toUpperCase, 0x0d) )
    private def stnl: Parser[(String, Int)]  = """(?i)STNL""".r  ^^ ( x => (x.toUpperCase, 0x0e) )
    private def opr: Parser[(String, Int)]   = """(?i)OPR""".r   ^^ ( x => (x.toUpperCase, 0x0f) )



    private def indirectInstruction: Parser[IndirectInstruction] =
        indirectOpcode ^^ {
            opcode =>
                if (debugParser) logger.debug("in indirectInstruction, opcode: " + opcode)
                IndirectInstruction(opcode._1, opcode._2)
        }

    private def indirectOpcode: Parser[(String, Array[Int])] = rev | lb | /* ... */ mul | /* ... */ fpumulby2 // etc.

    // Opcode to direct instructions. As per CWG, p 117
    private def short(opcode: Int): Array[Int] = Array(0xf0 | (opcode & 0x0f)) // OPR(opcode)
    private def long(opcode: Int): Array[Int] = {
        Array(0x20 | ((opcode & 0xf0) >> 4),  // PFIX(opcode left-nybble)
              0xf0 | (opcode & 0x0f))         // OPR(opcode right-nybble)
    }
    private def seq(opcode: Int): Array[Int] = {
        val areg = collection.mutable.ArrayBuffer[Int]()
        if (opcode > 0x0f) {
            // Two nybbles into Areg
            areg += (0x20 | ((opcode & 0xf0) >> 4)) // PFIX(opcode left-nybble)
        }
        // Single nybble into Areg
        areg += (0x40 | (opcode & 0x0f)) // LDC(opcode left-nybble)

        areg ++= long(0xAB) // FPENTRY
        areg.toArray
    }

    private def rev: Parser[(String, Array[Int])]       = """(?i)REV""".r       ^^ ( x => (x.toUpperCase, short(0x00)) )
    private def lb: Parser[(String, Array[Int])]        = """(?i)LB""".r        ^^ ( x => (x.toUpperCase, short(0x01)) )

    private def mul: Parser[(String, Array[Int])]       = """(?i)MUL""".r       ^^ ( x => (x.toUpperCase, long(0x53)) )

    private def fpumulby2: Parser[(String, Array[Int])] = """(?i)FPUMULBY2""".r ^^ ( x => (x.toUpperCase, seq(0x12)) )

}
