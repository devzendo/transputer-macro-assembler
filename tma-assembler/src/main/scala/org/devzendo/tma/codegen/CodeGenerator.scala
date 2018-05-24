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

import org.devzendo.tma.ast.AST.{Label, SymbolName}
import org.devzendo.tma.ast._
import org.log4s.Logger

import scala.collection.mutable

class CodeGenerator(debugCodegen: Boolean) {
    val logger: Logger = org.log4s.getLogger

    private val model = new AssemblyModel

    object GenerationMode extends Enumeration {
        val Assembly, If1Seen, ElseSeen = Value
    }
    private var generationMode = GenerationMode.Assembly

    private[codegen] val p2Structures = mutable.ArrayBuffer[Pass2Structure]()
    private[codegen] var currentP2Structure = new Pass2Structure()

    def processLine(line: Line): Unit = {
        // TODO what about collecting exceptions?
        try {
            if (generationMode == GenerationMode.ElseSeen && notEndif(line)) {
                logger.debug("Adding line to Pass 2 Collection: " + line)
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
            model.setLabel(label, model.getDollar, line.number)
        )
    }

    private def processLineStatement(line: Line): Unit = {
        line.stmt.foreach((stmt: Statement) =>
            processStatement(line, stmt)
        )
    }

    private def processStatement(line: Line, stmt: Statement): Unit = {
        val lineNumber = line.number
        logger.debug("Line " + lineNumber + " Statement: " + stmt)
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
            case Org(expr) => processOrg(lineNumber, expr)
            case ConstantAssignment(name, expr) => processConstantAssignment(lineNumber, name, expr)
            case VariableAssignment(name, expr) => processVariableAssignment(lineNumber, name, expr)
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
            case Else() => processElse(lineNumber)
            case Endif() => processEndif(lineNumber)
        }
    }

    private def processOrg(lineNumber: Int, expr: Expression): Unit = {
        if (expressionContainsCharacters(expr)) {
            throw new CodeGenerationException(lineNumber, "Origin cannot be set to a Character expression '" + expr + "'")
        }
        val either = model.evaluateExpression(expr)
        either match {
            case Left(undefineds) => throw new CodeGenerationException(lineNumber, "Undefined symbol(s) '" + undefineds.mkString(",") + "'")
            case Right(org) =>
                logger.debug("Org: " + org)
                model.setDollar(org, lineNumber)
        }
    }

    private def processConstantAssignment(lineNumber: Int, name: SymbolName, expr: Expression): Unit = {
        if (expressionContainsCharacters(expr)) {
            throw new CodeGenerationException(lineNumber, "Constant cannot be set to a Character expression '" + expr + "'")
        }
        val either = model.evaluateExpression(expr)
        // TODO throw on undefineds
        either match {
            case Left(undefineds) =>
            case Right(value) =>
                logger.debug("Constant " + name + " = " + value)
                model.setConstant(name, value, lineNumber)
        }
    }

    private def processVariableAssignment(lineNumber: Int, name: SymbolName, expr: Expression): Unit = {
        if (expressionContainsCharacters(expr)) {
            throw new CodeGenerationException(lineNumber, "Variable cannot be set to a Character expression '" + expr + "'")
        }
        val either = model.evaluateExpression(expr)
        // TODO throw on undefineds
        either match {
            case Left(undefineds) =>
            case Right(value) =>
                logger.debug("Variable " + name + " = " + value)
                model.setVariable(name, value, lineNumber)
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
        logger.debug("Setting Pass 2 start address of " + dollar + " in If1")
        // We'll need the pass 1 current address, when processing the pass 2 lines.
        currentP2Structure.setStartAddress(dollar)
        // Assembly of statements continues normally...
        generationMode = GenerationMode.If1Seen
    }

    private def processElse(lineNumber: Int): Unit = {
        if (generationMode != GenerationMode.If1Seen) {
            throw new CodeGenerationException(lineNumber, "Else seen without prior If1")
        }
        val dollar = model.getDollar
        logger.debug("Setting Pass 2 end address of " + dollar + " in Else; switching to Pass 2 Line Collection")
        // How large is the pass 1 block? Store end address (current address after its contents have been assembled)
        // in current pass 2 structure..
        currentP2Structure.setEndAddress(dollar)
        // Switch to collect statements/lines for pass 2
        generationMode = GenerationMode.ElseSeen
    }

    private def processEndif(lineNumber: Int): Unit = {
        if (generationMode != GenerationMode.If1Seen && generationMode != GenerationMode.ElseSeen) {
            throw new CodeGenerationException(lineNumber, "Endif seen without prior If1")
        }
        logger.debug("Storing collected Pass 2 Lines in Endif; switching to Pass 1 Line Assembly")
        // Collect the built pass 2 structure in a list for pass 2 processing..
        p2Structures += currentP2Structure
        // Create new current pass 2 structure
        currentP2Structure = new Pass2Structure
        // Switch back to assemble statements to storage/model updates in pass 1
        generationMode = GenerationMode.Assembly
    }

    private def pass2: Unit = {
        // iterate over list of pass 2 structures:
        //   set $ to the start address
        //   iterate over each stored line/statement
        //     pass it through processStatement to possibly append Storages at existing addresses (these'll be resolved
        //     sequentially, overwriting earlier memory, in the writers).
        //   get $; is it at the same place it was after assembling the pass 1 block? throw if not
    }

    def createModel(lines: List[Line]): AssemblyModel = {
        logger.debug("Creating model from " + lines.size + " line(s)")

        lines.foreach( (l: Line) => processLine(l) )

        logger.debug("Checking for unresolved forward references")
        model.checkUnresolvedForwardReferences() // will throw if there are any
        // pass2();

        model
    }
}
