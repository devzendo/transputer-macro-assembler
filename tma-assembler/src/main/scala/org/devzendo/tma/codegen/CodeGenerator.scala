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
import org.devzendo.tma.ast.AST.{Label, SymbolName}
import org.devzendo.tma.ast._
import org.log4s.Logger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CodeGenerator(debugCodegen: Boolean, model: AssemblyModel) {
    val logger: Logger = org.log4s.getLogger

    object GenerationMode extends Enumeration {
        val Assembly, If1Seen, ElseSeen = Value
    }
    private var generationMode = GenerationMode.Assembly

    private[codegen] val p2Structures = mutable.ArrayBuffer[Pass2Structure]()
    private[codegen] var currentP2Structure = new Pass2Structure()

    private var lastLineNumber = 0
    private var passNumber = 1

    def getLastLineNumber: Int = lastLineNumber

    private[codegen] var inputLines = mutable.ArrayBuffer[Line]()

    /* State maintained during converge mode - when building optimal encodings for direct instruction offsets of
     * forward-referenced symbols. Also see the AssemblyModel's converge mode flag that relaxes whether symbols can be
     * redefined.
     *
     * Consider a DirectInstruction containing one or more undefined symbols, and all subsequent Statements until
     * these symbols are defined. Note the line index of the start and end of this sequence.
     * Compute the minimal encodings of all DirectInstructions in the list, converting them into
     * DirectEncodedInstructions. The resulting list is then available to the CodeGenerator.
     * The list of subsequent Statements may contain further undefined symbols - the CodeGenerator will keep giving
     * Statements to this encoder until all undefined symbols have potential resolutions.
     *
     * The algorithm used here is from The Transputer Handbook, Graham & King, p48,49:
     *
     * "The solution is reasonably simple but time consuming. A data structure representing the whole program is built in
     * memory. Fixed length sections of code can be held as binary, but any label must be kept as a pointer to the label and
     * an associated size. Initially all offsets are assumed to fit in one nibble, with no prefixes needed. A pass over the
     * program is made, altering all those that require a larger offset to a suitable value. A further pass is then made,
     * expanding those instructions that do not now fit because the previous pass expanded instructions. This process
     * continues until no more changes need to be made.
     * This algorithm is the only one which is guaranteed to converge."
     *
     * need a DirectInstructionTrial that stores the length of its current encoding, and that can be asked if, when
     * evaluated, it will fit in that length. if not, it can have its encoding length increased by one byte. (then the loop
     * iterates). when all DITs return true, we're good, and can convert them to DirectEncodedInstructions
     * Each time round the loop, the StackedAssemblyModel's cached state is reset.
     *
     * DirectInstruction (from the Parser) -> (replaced in converge mode) DirectInstructionTrial -> (replaced when done) DirectEncodedInstruction
     *
     * DirectEncodedInstructions are then trivially generated into binary.
     */
    private var convergeMode: Boolean = false
    private var startConvergeLineIndex = 0
    private var endConvergeLineIndex = 0
    private var symbolsToConverge = mutable.HashSet[CasedSymbolName]()
    case class DirectInstructionState(directInstruction: DirectInstruction, currentSize: Int) {
    }
    private val directInstructionByLineIndex = mutable.HashMap[Int, DirectInstructionState]()
    private var startConvergeDollar = 0

    // End of converge mode state

    private val codeGenerationErrors = mutable.ArrayBuffer[CodeGenerationException]()

    // Chain of statement transformers
    type StatementTransformer = Statement => Statement
    private val statementTransformers = ArrayBuffer[StatementTransformer]()

    def addStatementTransformer(st: StatementTransformer): Unit = {
        statementTransformers += st
    }

    def createModel(lines: List[Line]): AssemblyModel = {
        // Store the lines in a mutable, random accessible form, as convergence needs them
        inputLines.clear()
        inputLines ++= lines

        logger.info("Pass 1: Creating model from " + lines.size + " macro-expanded line(s)")
        inputLines.zipWithIndex.foreach { tuple: (Line, Int) =>
            try {
                processLine(tuple._1, tuple._2)
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

        // TODO in converge mode still? there must be unresolveds then...

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

    private[codegen] def lineContainsDirectInstructionWithUndefinedSymbols(line: Line): Set[CasedSymbolName] = {
        line.stmt match {
            case Some(DirectInstruction(_, _, expr)) => model.findUndefineds(expr)
            case Some(_) => Set.empty
            case None => Set.empty
        }
    }

    private def processLine(line: Line, lineIndex: Int): Unit = {
        if (debugCodegen) {
            logger.info("Line " + line.number + ": " + line.toString)
        }

        if (line.number > lastLineNumber) {
            lastLineNumber = line.number
        }

        try {
            if (generationMode == GenerationMode.ElseSeen && notEndif(line)) {
                if (debugCodegen) {
                    logger.info("Adding line to Pass 2 Collection: " + line)
                }
                currentP2Structure.addPass2Line((line, lineIndex))
            } else {
                createLabel(line)

                val directUndefineds = lineContainsDirectInstructionWithUndefinedSymbols(line)
                if (directUndefineds.nonEmpty) {
                    if (!convergeMode) {
                        convergeMode = true
                        startConvergeDollar = model.getDollar
                        directInstructionByLineIndex.clear()
                        startConvergeLineIndex = lineIndex
                        logger.debug("Start of convergable lines at line index " + lineIndex + " line number " + line.number + " $=" + HexDump.int2hex(startConvergeDollar))
                    }
                    logger.debug("Adding " + directUndefineds + " to converge symbol set")
                    symbolsToConverge ++= directUndefineds
                }

                val (modifiedLine, maybeStatement) = applyStatementTransformers(line)
                model.addLine(modifiedLine)

                // Convergence replays Lines - so if the Statement has been transformed, it must be replaced in its
                // Line in the input..
                // TODO this is an appalling code smell. too much mutable state...
                inputLines(lineIndex) = modifiedLine

                maybeStatement.foreach {
                    stmt: Statement => processStatement(modifiedLine, lineIndex, stmt)
                }

                // Has convergence ended? Resolve any label on this line.
                modifiedLine.label.foreach((label: Label) => {
                    resolveConvergeSetSymbol(CasedSymbolName(label))
                })

                if (convergeMode && symbolsToConverge.isEmpty) {
                    endConvergeLineIndex = lineIndex
                    logger.debug("End of convergable lines on line index " + lineIndex + " line number " + modifiedLine.number)
                    converge()
                    convergeMode = false
                }
            }

            if (debugCodegen) {
                logger.debug("")
            }

        } catch {
            case ame: AssemblyModelException => throw new CodeGenerationException(line.number, ame.getMessage)
        }
    }

    private def resolveConvergeSetSymbol(casedSymbolName: CasedSymbolName) = {
        if (symbolsToConverge.contains(casedSymbolName)) {
            logger.debug("Removing " + casedSymbolName + " from converge symbol set")
            symbolsToConverge.remove(casedSymbolName)
        }
    }

    def setOfLineNumbersInConvergence(): List[Int] = {
        val set = mutable.HashSet[Int]()
        for (i <- startConvergeLineIndex to endConvergeLineIndex) {
            val line = inputLines(i)
            set += line.number
        }
        set.toList
    }

    private def converge(): Unit = {
        if (debugCodegen) {
            logger.info("Converging line indices [" + startConvergeLineIndex + " .. " + endConvergeLineIndex + "] Start $ " + HexDump.int2hex(startConvergeDollar))
        }
        model.setConvergeMode(true)
        var iteration = 0
        val lineNumbersInConvergence = setOfLineNumbersInConvergence()
        if (debugCodegen) {
            logger.info("Line numbers in convergence: " + lineNumbersInConvergence)
        }

        var again = false
        do {
            iteration += 1
            again = false
            model.setDollarSilently(startConvergeDollar)
            if (debugCodegen) {
                logger.info("Convergence iteration " + iteration)
            }

            // At top of loop, clear down model storage for all lines - macro expansions mean that multiple entries
            // in inputLines could have the same line number. So only clear each line once, before reprocessing them
            // all, below.
            lineNumbersInConvergence.foreach(lineNumber => model.clearSourcedValuesForLineNumber(lineNumber))
            // Convergence should only occur in pass 1. Pass 2 could add Storages that this would clear.

            for (lineIndex <- startConvergeLineIndex to endConvergeLineIndex) {
                val line = inputLines(lineIndex)
                if (debugCodegen) {
                    logger.info("Converging line index " + lineIndex + ": " + line)
                }

                createLabel(line) // update any label with current $
                val maybeElement = directInstructionByLineIndex.get(lineIndex)
                maybeElement match {
                    case Some(DirectInstructionState(di: DirectInstruction, currentSize: Int)) =>
                        if (debugCodegen) {
                            logger.debug("Current size for direct instruction: " + currentSize)
                        }
                        model.evaluateExpression(di.expr) match {

                            case Right(value) =>
                                if (debugCodegen) {
                                    logger.debug("Defined: Encoding value " + value)
                                }

                                // This evaluation needs to take the encoded instruction length into account, when a
                                // Unary(OffsetFrom(x)) is in the expression. Here and in non-convergent evaluation.
                                val valueToEncode = encodeOffsetValue(di, value)

                                val encoded = DirectInstructionEncoder.apply(di.opbyte, valueToEncode)
                                if (debugCodegen) {
                                    logger.debug(s"Defined: Encoding direct instruction (convergence); original value to encode $value; after length adjustment $valueToEncode")
                                    logger.debug("Defined: New encoded size for direct instruction: " + encoded.size)
                                }
                                if (encoded.size > currentSize) {
                                    if (debugCodegen) {
                                        logger.debug("Defined: Another byte of storage required")
                                    }
                                    // requires more size
                                    directInstructionByLineIndex.put(lineIndex, DirectInstructionState(di, currentSize + 1))
                                    model.incrementDollar(currentSize + 1)
                                    again = true
                                } else {
                                    if (debugCodegen) {
                                        logger.debug("Defined: Storage size ok; allocating")
                                    }
                                    model.allocateStorageForLine(line, 1, encoded map Number) // silently increments $
                                }

                            case Left(undefineds) =>
                                if (debugCodegen) {
                                    logger.debug("Undefined: Storage size static: Symbol(s) (" + undefineds + ") are not yet defined; allocating 1 byte")
                                }
                                model.incrementDollar(currentSize)
                        }

                    case None =>
                        if (debugCodegen) {
                            logger.debug("Processing non-direct-instruction")
                        }
                        // NB Not processLineStatement as that adds the Line to the model, and it's already been added once.
                        line.stmt.foreach((stmt: Statement) =>
                            processStatement(line, lineIndex, stmt)
                        )
                }
            }
        } while (again)
        if (debugCodegen) {
            logger.info("Convergence complete after " + iteration + " iteration(s)")
        }
        model.setConvergeMode(false)
    }

    private def encodeOffsetValue(di: DirectInstruction, value: Int) = {
        di.expr match {
            case Unary(op, _) =>
                op match {
                    case OffsetFrom(_) => value - DirectInstructionEncoder.lengthOfEncodedOffsetFromOpcodeInstruction(value)
                    case _ => value
                }
            case _ => value
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
            model.setLabel(CasedSymbolName(label), model.getDollar, line)
        )
    }

    private def applyStatementTransformers(line: Line): (Line, Option[Statement]) = {
        line.stmt match {
            case Some(initialStmt) =>
                // Apply all statement transformers to the statement...
                try {

                    val stmt = statementTransformers.foldLeft(initialStmt) {
                        (prevStmt: Statement, transformer: StatementTransformer) => transformer(prevStmt)
                    }

                    if (stmt != initialStmt) {
                        if (debugCodegen) {
                            logger.debug("Line " + line.number + " (Transformed): " + stmt)
                        }
                        val replacedLine = line.copy(stmt = Some(stmt))
                        (replacedLine, Some(stmt))
                    } else {
                        (line, Some(stmt))
                    }

                } catch {
                    case ste: StatementTransformationException =>
                        logger.debug(s"Rethowing ${ste.getMessage}")
                        throw new CodeGenerationException(line.number, ste.getMessage)
                }
            case None =>
                (line, None)
        }
    }

    private def processStatement(line: Line, lineIndex: Int, stmt: Statement): Unit = {
        val lineNumber = line.number

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
                    case "TRANSPUTER" => Endianness.Little
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
            case DBDup(_, _) => // Will have been transformed into DB by OffsetTransformer
            case DWDup(_, _) => // Will have been transformed into DW by OffsetTransformer
            case DDDup(_, _) => // Will have been transformed into DD by OffsetTransformer
            case If1() => processIf1()
            case Else() => processElse(line)
            case Endif() => processEndif(line)
            case DirectInstruction(_, opbyte, expr) => processDirectInstruction(line, lineIndex, stmt.asInstanceOf[DirectInstruction], opbyte, expr)
            case DirectEncodedInstruction(_, opbytes) => model.allocateInstructionStorageForLine(line, opbytes)
            case IndirectInstruction(_, opbytes) => model.allocateInstructionStorageForLine(line, opbytes)
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
            case Left(undefinedSymbols) =>
                throw new CodeGenerationException(lineNumber, "Undefined symbol(s) '" + undefinedSymbols.mkString(",") + "'")
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
        val casedName = CasedSymbolName(name)
        val lineNumber = line.number
        if (expressionContainsCharacters(expr)) {
            throw new CodeGenerationException(lineNumber, "Constant cannot be set to a Character expression '" + expr + "'")
        }
        val either = model.evaluateExpression(expr)
        either match {
            case Left(undefinedSymbols) =>
                if (debugCodegen) {
                    logger.info("Cannot set constant " + casedName + " to expression " + expr + " due to undefined symbols " + undefinedSymbols + " on line number " + lineNumber)
                }
                model.recordSymbolForwardReferences(undefinedSymbols, casedName, expr, line, SymbolType.Constant)
            case Right(value) =>
                model.setConstant(casedName, value, line)
                resolveConvergeSetSymbol(casedName)
        }
    }

    private def processVariableAssignment(line: Line, name: SymbolName, expr: Expression): Unit = {
        val casedName = CasedSymbolName(name)
        val lineNumber = line.number
        if (expressionContainsCharacters(expr)) {
            throw new CodeGenerationException(lineNumber, "Variable cannot be set to a Character expression '" + expr + "'")
        }
        val either = model.evaluateExpression(expr)
        either match {
            case Left(undefineds) =>
                if (debugCodegen) {
                    logger.info("Cannot set variable " + casedName + " to expression " + expr + " due to undefined symbols " + undefineds + " on line number " + lineNumber)
                }
                model.recordSymbolForwardReferences(undefineds, casedName, expr, line, SymbolType.Variable)
            case Right(value) =>
                model.setVariable(casedName, value, line)
                resolveConvergeSetSymbol(casedName)
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
                    processLine(line._1, line._2)
                }

                // Current address must match end address of pass 1. If not, the blocks are different sizes.
                val endAddressPass2 = model.getDollar
                if (endAddressPass2 != p2.getEndAddress) {
                    throw new CodeGenerationException(p2Lines.head._1.number, "Differently-sized blocks in Passes 1 and 2: Pass 1=" +
                      (p2.getEndAddress - p2.getStartAddress) + " byte(s); Pass 2=" +
                      (endAddressPass2 - p2.getStartAddress) + " byte(s)")
                }
            }
        }
    }

    private def processDirectInstruction(line: Line, lineIndex: Int, di: DirectInstruction, opbyte: Int, expr: Expression): Unit = {
        val lineNumber = line.number
        val evaluation = model.evaluateExpression(expr)
        evaluation match {
            case Right(value) =>

                // This evaluation needs to take the encoded instruction length into account, when a
                // Unary(OffsetFrom(x)) is in the expression. Here and in convergent evaluation.
                val valueToEncode = encodeOffsetValue(di, value)

                logger.debug(s"Encoding direct instruction (non-convergence); original value to encode $value; after length adjustment $valueToEncode")
                val prefixedBytes = DirectInstructionEncoder.apply(opbyte, valueToEncode)
                model.allocateInstructionStorageForLine(line, prefixedBytes)
            case Left(undefineds) =>
                if (debugCodegen) {
                    logger.info("Symbol(s) (" + undefineds + ") are not yet defined on line " + lineNumber)
                    logger.info("Storing undefined direct instruction " + di + " on line index " + lineIndex)
                }
                directInstructionByLineIndex.put(lineIndex, DirectInstructionState(di, 1))
                model.incrementDollar(1)
        }
    }
}
