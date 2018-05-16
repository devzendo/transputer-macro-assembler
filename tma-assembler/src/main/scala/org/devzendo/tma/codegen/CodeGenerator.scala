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

import org.devzendo.tma.ast.AST.SymbolName
import org.devzendo.tma.ast._
import org.log4s.Logger

class CodeGenerator(debugCodegen: Boolean) {
    val logger: Logger = org.log4s.getLogger

    private val model = new AssemblyModel

    def processLine(line: Line): Unit = {
        // TODO what about collecting exceptions?
        // TODO what about labels?
        try {
            line.stmt.foreach ( (stmt: Statement) =>
                processStatement(line.number, stmt)
            )
        } catch {
            case ame: AssemblyModelException => throw new CodeGenerationException(line.number, ame.getMessage)
        }
    }

    private def processStatement(lineNumber: Int, stmt: Statement) = {
        stmt match {
            case Title(text) => {
                model.title = text
                logger.debug("Title is '" + text + "'")
            }
            case Page(rows, columns) => {
                model.rows = rows
                model.columns = columns
                logger.debug("Rows: " + rows + " Columns: " + columns)
            }
            case Processor(name) => {
                model.processor = Some(name)
                logger.debug("Processor is '" + name + "'")
            }
            case Org(expr) => processOrg(lineNumber, expr)
            case ConstantAssignment(name, expr) => processConstantAssignment(lineNumber, name, expr)
            case VariableAssignment(name, expr) => processVariableAssignment(lineNumber, name, expr)
            case Ignored() => // Do nothing
        }
    }

    private def processOrg(lineNumber: Int, expr: Expression) = {
        val either = model.evaluateExpression(expr)
        // TODO throw on undefineds
        either match {
            case Left(undefineds) =>
            case Right(n) => {
                logger.debug("Org: " + n)
                model.setDollar(n, lineNumber)
            }
        }
    }

    private def processConstantAssignment(lineNumber: Int, name: SymbolName, expr: Expression) = {
        val either = model.evaluateExpression(expr)
        // TODO throw on undefineds
        either match {
            case Left(undefineds) =>
            case Right(value) => {
                logger.debug("Constant " + name + " = " + value)
                model.setConstant(name, value, lineNumber)
            }
        }
    }

    private def processVariableAssignment(lineNumber: Int, name: SymbolName, expr: Expression) = {
        val either = model.evaluateExpression(expr)
        // TODO throw on undefineds
        either match {
            case Left(undefineds) =>
            case Right(value) => {
                logger.debug("Variable " + name + " = " + value)
                model.setVariable(name, value, lineNumber)
            }
        }
    }

    def createModel(lines: List[Line]): AssemblyModel = {
        logger.debug("Creating model from " + lines.size + " line(s)")

        lines.foreach( (l: Line) => processLine(l) )
        model
    }
}
