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

import org.devzendo.commoncode.string.HexDump
import org.devzendo.tma.ast.AST.{Label, Opcode, SymbolName}
import org.devzendo.tma.ast._
import org.log4s.Logger

import scala.collection.mutable

class CodeGenerator(debugCodegen: Boolean) {
    val logger: Logger = org.log4s.getLogger

    private val model = new AssemblyModel(debugCodegen)

    object GenerationMode extends Enumeration {
        val Assembly, If1Seen, ElseSeen = Value
    }
    private var generationMode = GenerationMode.Assembly

    private[codegen] val p2Structures = mutable.ArrayBuffer[Pass2Structure]()
    private[codegen] var currentP2Structure = new Pass2Structure()

    private var lastLineNumber = 0
    private var passNumber = 1

    def getLastLineNumber: Int = lastLineNumber

    def processLine(line: Line): Unit = {
        if (line.number > lastLineNumber) {
            lastLineNumber = line.number
        }

        try {
            if (generationMode == GenerationMode.ElseSeen && notEndif(line)) {
                if (debugCodegen) {
                    logger.info("Adding line to Pass 2 Collection: " + line)
                }
                currentP2Structure.addPass2Line(line)
            } else {
                createLabel(line)
                processLineStatement(line)
            }
        } catch {
            case ame: AssemblyModelException => throw new CodeGenerationException(line.number, ame.getMessage)
        }
    }

    private def notEndif(line: Line): Boolean = {
        line.stmt match {
            case Some(Endif()) => false
            case _ => true
        }
    }

    private def createLabel(line: Line): Unit = {
        line.label.foreach((label: Label) =>
            model.setLabel(label, model.getDollar, line)
        )
    }

    private def processLineStatement(line: Line): Unit = {
        model.addLine(line)
        line.stmt.foreach((stmt: Statement) =>
            processStatement(line, stmt)
        )
    }

    private def processStatement(line: Line, stmt: Statement): Unit = {
        val lineNumber = line.number
        if (debugCodegen) {
            logger.info("Line " + lineNumber + " Statement: " + stmt)
        }

        // Pass 2 fixups run after pass 1 (duh!), and require processing of statements after this check would have
        // triggered in pass 1.
        if (model.hasEndBeenSeen && passNumber == 1) {
            throw new CodeGenerationException(lineNumber, "No statements allowed after End statement")
        }

        stmt match {
            case Title(text) =>
                model.title = text
                logger.debug("Title is '" + text + "'")
            case Page(rows, columns) =>
                model.rows = rows
                model.columns = columns
                logger.debug("Rows: " + rows + " Columns: " + columns)
            case Processor(name) =>
                model.processor = Some(name)
                logger.debug("Processor is '" + name + "'")
                model.endianness = name match {
                    case "386" => Endianness.Little
                    case "T800" => Endianness.Little
                }
            case Align(n) => processAlign(line, n)
            case Org(expr) => processOrg(line, expr)
            case End(expr) => processEnd(line, expr)
            case ConstantAssignment(name, expr) => processConstantAssignment(line, name, expr)
            case VariableAssignment(name, expr) => processVariableAssignment(line, name, expr)
            case Ignored() => // Do nothing
            case MacroStart(_, _) =>  // All macro AST statements are handled by the parser; the expansions are handled
            case MacroBody(_) =>      // by the rest of the AST statement handlers, here..
            case MacroEnd() =>        // So, do nothing...
            case MacroInvocation(_, _) => // Non-macro statements would follow after an invocation, unless it's an empty macro.
            case DB(exprs) => model.allocateStorageForLine(line, 1, exprs)
            case DW(exprs) => model.allocateStorageForLine(line, 2, exprs)
            case DD(exprs) => model.allocateStorageForLine(line, 4, exprs)
            case DBDup(count, repeatedExpr) => model.allocateStorageForLine(line, 1, count, repeatedExpr)
            case DWDup(count, repeatedExpr) => model.allocateStorageForLine(line, 2, count, repeatedExpr)
            case DDDup(count, repeatedExpr) => model.allocateStorageForLine(line, 4, count, repeatedExpr)
            case If1() => processIf1()
            case Else() => processElse(line)
            case Endif() => processEndif(line)
            case DirectInstruction(opcode, opbyte, expr) => processDirectInstruction(line, opcode, opbyte, expr)
            case IndirectInstruction(opcode, opbytes) => processIndirectInstruction(line, opcode, opbytes)
        }
    }

    private def processAlign(line: Line, alignment: Int): Unit = {
        val dollar = model.getDollar
        val remainder = dollar % alignment
        if (remainder > 0) {
            val newDollar = alignment - remainder
            if (debugCodegen) {
                logger.info("Align: from " + HexDump.int2hex(dollar) + " to " + HexDump.int2hex(newDollar))
            }
            model.incrementDollar(newDollar)
        }
    }

    private def processOrg(line: Line, expr: Expression): Unit = {
        val lineNumber = line.number
        if (expressionContainsCharacters(expr)) {
            throw new CodeGenerationException(lineNumber, "Origin cannot be set to a Character expression '" + expr + "'")
        }
        val either = model.evaluateExpression(expr)
        either match {
            case Left(undefineds) => throw new CodeGenerationException(lineNumber, "Undefined symbol(s) '" + undefineds.mkString(",") + "'")
            case Right(org) =>
                if (debugCodegen) {
                    logger.info("Org: " + HexDump.int2hex(org))
                }
                model.setDollar(org, line)
        }
    }

    private def processEnd(line: Line, expr: Option[Expression]): Unit = {
        model.endHasBeenSeen()
    }

    private def processConstantAssignment(line: Line, name: SymbolName, expr: Expression): Unit = {
        val lineNumber = line.number
        if (expressionContainsCharacters(expr)) {
            throw new CodeGenerationException(lineNumber, "Constant cannot be set to a Character expression '" + expr + "'")
        }
        val either = model.evaluateExpression(expr)
        either match {
            case Left(undefineds) =>
                if (debugCodegen) {
                    logger.info("Cannot set constant " + name + " to expression " + expr + " due to undefined symbols " + undefineds + " on line number " + lineNumber)
                }
                model.recordSymbolForwardReferences(undefineds, name, expr, line, UnresolvableSymbolType.Constant)
            case Right(value) =>
                model.setConstant(name, value, line)
        }
    }

    private def processVariableAssignment(line: Line, name: SymbolName, expr: Expression): Unit = {
        val lineNumber = line.number
        if (expressionContainsCharacters(expr)) {
            throw new CodeGenerationException(lineNumber, "Variable cannot be set to a Character expression '" + expr + "'")
        }
        val either = model.evaluateExpression(expr)
        either match {
            case Left(undefineds) =>
                if (debugCodegen) {
                    logger.info("Cannot set variable " + name + " to expression " + expr + " due to undefined symbols " + undefineds + " on line number " + lineNumber)
                }
                model.recordSymbolForwardReferences(undefineds, name, expr, line, UnresolvableSymbolType.Variable)
            case Right(value) =>
                model.setVariable(name, value, line)
        }
    }

    private def expressionContainsCharacters(expr: Expression): Boolean = {
        expr match {
            case SymbolArg(_) => false
            case Number(_) => false
            case Characters(_) => true
            case Unary(_, uExpr) => expressionContainsCharacters(uExpr)
            case Binary(_, lExpr, rExpr) => expressionContainsCharacters(lExpr) || expressionContainsCharacters(rExpr)
        }
    }

    private def processIf1(): Unit = {
        val dollar = model.getDollar
        if (debugCodegen) {
            logger.info("Setting Pass 2 start address of " + dollar + " in If1")
        }
        // We'll need the pass 1 current address, when processing the pass 2 lines.
        currentP2Structure.setStartAddress(dollar)
        // Assembly of statements continues normally...
        generationMode = GenerationMode.If1Seen
    }

    private def processElse(line: Line): Unit = {
        if (generationMode != GenerationMode.If1Seen) {
            throw new CodeGenerationException(line.number, "Else seen without prior If1")
        }
        val dollar = model.getDollar
        if (debugCodegen) {
            logger.info("Setting Pass 2 end address of " + dollar + " in Else; switching to Pass 2 Line Collection")
        }
        // How large is the pass 1 block? Store end address (current address after its contents have been assembled)
        // in current pass 2 structure..
        currentP2Structure.setEndAddress(dollar)
        // Switch to collect statements/lines for pass 2
        generationMode = GenerationMode.ElseSeen
    }

    private def processEndif(line: Line): Unit = {
        if (generationMode != GenerationMode.If1Seen && generationMode != GenerationMode.ElseSeen) {
            throw new CodeGenerationException(line.number, "Endif seen without prior If1")
        }
        if (debugCodegen) {
            logger.info("Storing collected Pass 2 Lines in Endif; switching to Pass 1 Line Assembly")
        }
        // Collect the built pass 2 structure in a list for pass 2 processing..
        p2Structures += currentP2Structure
        // Create new current pass 2 structure
        currentP2Structure = new Pass2Structure
        // Switch back to assemble statements to storage/model updates in pass 1
        generationMode = GenerationMode.Assembly
    }

    private def pass2(): Unit = {
        passNumber = 2
        for (p2 <- p2Structures) {
            val p2Lines = p2.getPass2Lines
            // Only bother processing lines, and setting $ if there are any lines - can't set $ without a line number
            // for diagnostics..
            if (p2Lines.nonEmpty) {
                model.setDollarSilently(p2.getStartAddress)
                for (line <- p2Lines) {
                    // This will possibly append Storages at existing addresses - these will be resolved sequentially
                    // overwriting earlier memory, in the writers.
                    processLine(line)
                }

                // Current address must match end address of pass 1. If not, the blocks are different sizes.
                val endAddressPass2 = model.getDollar
                if (endAddressPass2 != p2.getEndAddress) {
                    throw new CodeGenerationException(p2Lines.head.number, "Differently-sized blocks in Passes 1 and 2: Pass 1=" +
                      (p2.getEndAddress - p2.getStartAddress) + " byte(s); Pass 2=" +
                      (endAddressPass2 - p2.getStartAddress) + " byte(s)")
                }
            }
        }
    }

    private def processDirectInstruction(line: Line, opcode: Opcode, opbyte: Int, expr: Expression): Unit = {
        model.allocateEvaluatedInstructionStorageForLine(line, opbyte, expr)
    }

    private def processIndirectInstruction(line: Line, opcode: Opcode, opbytes: List[Int]): Unit = {
        model.allocateInstructionStorageForLine(line, opbytes)
    }

    private val codeGenerationErrors = mutable.ArrayBuffer[CodeGenerationException]()

    def createModel(lines: List[Line]): AssemblyModel = {
        logger.info("Pass 1: Creating model from " + lines.size + " macro-expanded line(s)")
        lines.foreach { l: Line =>
            try {
                processLine(l)
            } catch {
                case cge: CodeGenerationException => codeGenerationErrors += cge
            }
        }

        logger.info("End of Pass 1: Checking for unresolved forward references")
        try {
            model.checkUnresolvedForwardReferences() // will throw if there are any
        } catch {
            case cge: CodeGenerationException => codeGenerationErrors += cge // doesn't throw these
            case ame: AssemblyModelException => codeGenerationErrors += new CodeGenerationException(0, ame.getMessage)
        }

        logger.info("Pass 2: Updating model with " + p2Structures.size + " pass 2 section(s)")
        try {
            pass2()
        } catch {
            case cge: CodeGenerationException => codeGenerationErrors += cge
        }
        logger.info("End of Pass 2")

        model
    }

    def getCodeGenerationExceptions: List[CodeGenerationException] = codeGenerationErrors.toList

    def endCheck(): Unit = {
        if (!model.hasEndBeenSeen) {
            codeGenerationErrors += new CodeGenerationException(lastLineNumber, "End of input reached with no End statement")
        }
    }
}
