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

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
 * A mutable structure holding the output of the CodeGenerator.
 */
class AssemblyModel {
    val logger: Logger = org.log4s.getLogger

    private val dollar = "$"

    var title = ""
    var rows = 25
    var columns = 80
    var processor: Option[String] = None

    case class Value(value: Int, definitionLine: Int)

    private val variables = mutable.HashMap[String, Value]()
    private val constants = mutable.HashMap[String, Value]()
    private val labels = mutable.HashMap[String, Value]()

    // Macro expansions have the same line number as their invocation; hence line number -> list[line,storage?]
    case class Storage(address: Int, cellWidth: Int, data: Array[Int], line: Line)
    // Storage has a reference to its Line, so when the map of Undefined forward references -> Set[Storage]
    // is scanned at the end of the codegen phase, each Storage can show the Line on which the forward reference is.
    case class LineStorage(line: Line, storage: Option[Storage])
    private val storageForLines = mutable.HashMap[Int, mutable.ArrayBuffer[LineStorage]]() // indexed by line number
    // And it's a map, since it's likely to be sparsely populated (not every line generates Storage)

    setVariable(dollar, 0, 0)

    def getDollar: Int = getVariable(dollar)
    def setDollar(n: Int, lineNumber: Int): Unit = {
        setVariable(dollar, n, lineNumber)
    }

    def getVariable(name: String): Int = {
        variables.get(name) match {
            case Some(vr) => vr.value
            case None => throw new AssemblyModelException("Variable '" + name + "' has not been defined")
        }
    }
    def variable(name: String): Option[Int] = variables.get(name) match {
        case Some(con) => Some(con.value)
        case None => None
    }

    def setVariable(name: String, n: Int, lineNumber: Int): Unit = {
        constants.get(name) match {
            case Some(con) => throw new AssemblyModelException("Variable '" + name + "' cannot override existing constant; initially defined on line " + con.definitionLine)
            case None => // drop through
        }
        labels.get(name) match {
            case Some(label) => throw new AssemblyModelException("Variable '" + name + "' cannot override existing label; initially defined on line " + label.definitionLine)
            case None => // drop through
        }
        variables.put(name, Value(n, lineNumber))
        logger.debug("Variable " + name + " = " + n)
    }

    def getConstant(name: String): Int = {
        constants.get(name) match {
            case Some(con) => con.value
            case None => throw new AssemblyModelException("Constant '" + name + "' has not been defined")
        }
    }
    def constant(name: String): Option[Int] = constants.get(name) match {
        case Some(con) => Some(con.value)
        case None => None
    }
    def setConstant(name: String, n: Int, lineNumber: Int): Unit = {
        variables.get(name) match {
            case Some(vr) => throw new AssemblyModelException("Constant '" + name + "' cannot override existing variable; last stored on line " + vr.definitionLine)
            case None => // drop through
        }
        labels.get(name) match {
            case Some(label) => throw new AssemblyModelException("Constant '" + name + "' cannot override existing label; initially defined on line " + label.definitionLine)
            case None => // drop through
        }
        constants.get(name) match {
            case Some(con) => throw new AssemblyModelException("Constant '" + name + "' cannot be redefined; initially defined on line " + con.definitionLine)
            case None => {
                constants.put(name, Value(n, lineNumber))
                logger.debug("Constant " + name + " = " + n)
            }
        }
    }

    def getLabel(name: String): Int = {
        labels.get(name) match {
            case Some(con) => con.value
            case None => throw new AssemblyModelException("Label '" + name + "' has not been defined")
        }
    }
    def label(name: String): Option[Int] = labels.get(name) match {
        case Some(label) => Some(label.value)
        case None => None
    }
    def setLabel(name: String, n: Int, lineNumber: Int): Unit = {
        variables.get(name) match {
            case Some(vr) => throw new AssemblyModelException("Label '" + name + "' cannot override existing variable; last stored on line " + vr.definitionLine)
            case None => // drop through
        }
        constants.get(name) match {
            case Some(con) => throw new AssemblyModelException("Label '" + name + "' cannot override existing constant; initially defined on line " + con.definitionLine)
            case None => // drop through
        }
        labels.get(name) match {
            case Some(label) => throw new AssemblyModelException("Label '" + name + "' cannot be redefined; initially defined on line " + label.definitionLine)
            case None => {
                labels.put(name, Value(n, lineNumber))
                logger.debug("Label " + name + " = " + n)
            }
        }
    }


    /**
      * Evaluate an expression, returning Left(Set(undefined variable names)) or Right(value)
      *
      * @param expr some expression, of any complexity
      * @return undefined variable names, or the evaluated value.
      */
    def evaluateExpression(expr: Expression): Either[Set[String], Int] = {
        logger.debug("Evaluating " + expr)
        val undefineds = findUndefineds(expr)
        if (undefineds.nonEmpty) {
            logger.debug("Undefined symbols: " + undefineds)
            Left(undefineds)
        } else {
            val value = evaluateExpressionWithNoUndefineds(expr)
            logger.debug("Evaluation of " + expr + " = " + value)
            Right(value)
        }
    }

    // precondition: all SymbolArgs here are defined as a variable/constant/label
    private def evaluateExpressionWithNoUndefineds(expr: Expression): Int = {
        expr match {
            case SymbolArg(name) => lookupValue(name)
            case Number(n) => n
            case Characters(_) => throw new AssemblyModelException("Cannot evaluate '" + expr + "' as an Int")
            case Unary(op, uExpr) => evaluateUnary(op, uExpr)
            case Binary(op, lExpr, rExpr) => evaluateBinary(op, lExpr, rExpr)
        }
    }

    // precondition: name is defined as a variable/constant/label
    private def lookupValue(name: SymbolName): Int = {
        def getFallback(map: mutable.HashMap[String, Value], key: String)(fallback: => Int): Int = {
            map.get(key) match {
                case Some(x) => x.value
                case None => fallback
            }
        }
        getFallback(variables, name) (getFallback(constants, name) (getFallback(labels, name) ({
            throw new IllegalStateException("Precondition violation: " + name + " is supposed to be present as a variable/constant/label")
        })))
    }

    def definedValue(name: SymbolName): Boolean = {
        variables.contains(name) || constants.contains(name) || labels.contains(name)
    }

    def findUndefineds(expr: Expression): Set[String] = {
        expr match {
            case SymbolArg(name) => if (definedValue(name)) Set.empty else Set(name)
            case Number(_) => Set.empty
            case Characters(_) => Set.empty
            case Unary(_, uExpr) => findUndefineds(uExpr)
            case Binary(_, lExpr, rExpr) => findUndefineds(lExpr) ++ findUndefineds(rExpr)
        }
    }

    // precondition: expr has no undefineds
    private def evaluateUnary(op: Operator, expr: Expression): Int = {
        val value = evaluateDefinedExpression(expr)
        op match {
            case Negate() => value * -1
            case Not() => ~ value
            case _ => throw new IllegalStateException("Parser has passed an operation of " + op + " to a Unary")
        }
    }

    private def evaluateDefinedExpression(expr: Expression): Int = {
        evaluateExpression(expr) match {
            case Right(value) => value
            case Left(_) => throw new IllegalStateException("Precondition violation: " + expr + " contains undefined symbols")
        }
    }

    // precondition: lExpr, rExpr have no undefineds
    private def evaluateBinary(op: Operator, lExpr: Expression, rExpr: Expression): Int = {
        val lValue = evaluateDefinedExpression(lExpr)
        val rValue = evaluateDefinedExpression(rExpr)
        op match {
            case Add() => lValue + rValue
            case Sub() => lValue - rValue
            case Mult() => lValue * rValue
            case Div() => lValue / rValue
            case ShiftLeft() => lValue << rValue
            case ShiftRight() => lValue >> rValue
            case And() => lValue & rValue
            case Or() => lValue | rValue
            case Xor() => lValue ^ rValue
            case _ => throw new IllegalStateException("Parser has passed an operation of " + op + " to a Binary")
        }
    }

    def getStoragesForLine(lineNumber: Int): List[LineStorage] = {
        storageForLines(lineNumber).toList
    }

    def allocateStorageForLine(line: Line, cellWidth: Int, exprs: List[Expression]): Storage = {
        val linesList = if (storageForLines.contains(line.number)) {
            storageForLines(line.number)
        } else {
            val newLines = mutable.ArrayBuffer[LineStorage]()
            storageForLines.put(line.number, newLines)
            newLines
        }
        val storage = Storage(getDollar, cellWidth, Array.ofDim[Int](exprs.size), line)
        linesList += LineStorage(line, Some(storage))

        exprs.zipWithIndex.foreach((tuple: (Expression, Int)) => {
            evaluateExpression(tuple._1) match {
                case Right(value) => storage.data(tuple._2) = value
                case Left(undefineds) => // do nothing yet TODO undefineds
            }
        })

        storage
    }
}
