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

    // Macro expansions have the same line number as their invocation; hence line number -> list[storage+]
    case class Storage(address: Int, cellWidth: Int, data: Array[Int], line: Line, exprs: List[Expression])
    // Storage has a reference to its Line, so when the map of Undefined forward references -> Set[Storage]
    // is scanned at the end of the codegen phase, each Storage can show the Line on which the forward reference is.
    private val storagesForLines = mutable.HashMap[Int, mutable.ArrayBuffer[Storage]]() // indexed by line number
    // And it's a map, since it's likely to be sparsely populated (not every line generates Storage)

    // Any forward references are noted here, and resolution done on definition of the forwardly-referenced symbol
    // (either a variable, constant, or label).
    private val forwardReferenceFixups = mutable.HashMap[String, mutable.HashSet[Storage]]()

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
        resolveForwardReferences(name, n)
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
            case None =>
                constants.put(name, Value(n, lineNumber))
                logger.debug("Constant " + name + " = " + n)
                resolveForwardReferences(name, n)
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
            case None =>
                labels.put(name, Value(n, lineNumber))
                logger.debug("Label " + name + " = " + n)
                resolveForwardReferences(name, n)
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

    def containsUndefineds(expr: Expression): Boolean = {
        findUndefineds(expr).nonEmpty
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

    def getStoragesForLine(lineNumber: Int): List[Storage] = {
        storagesForLines(lineNumber).toList
    }

    private def getUnsignedInt(x: Int): Long = x & 0x00000000ffffffffL

    private def validateDataSizes(lineNumber: Int, data: Array[Int], cellWidth: Int): Unit = {
        val (max, name) = cellWidth match {
            case 1 => (0xffL, "BYTE")
            case 2 => (0xffffL, "WORD")
            case 4 => (0xffffffffL, "DWORD")
        }
        for (d <- data) {
            val dUnsigned = getUnsignedInt(d)
            logger.debug("cellWidth " + cellWidth + "; max " + max + "; name " + name + "; data " + dUnsigned)
            if (dUnsigned < 0 || dUnsigned > max) {
                throw new AssemblyModelException("Value of " + dUnsigned + " cannot be expressed in a " + name + " on line " + lineNumber)
            }
        }
    }

    def allocateStorageForLine(line: Line, cellWidth: Int, exprs: List[Expression]): Storage = {
        // TODO Orcish manoevre?
        val storages = if (storagesForLines.contains(line.number)) {
            storagesForLines(line.number)
        } else {
            val newLines = mutable.ArrayBuffer[Storage]()
            storagesForLines.put(line.number, newLines)
            newLines
        }
        val storage = Storage(getDollar, cellWidth, Array.ofDim[Int](exprs.size), line, exprs)
        storages += storage

        // Evaluate expressions, storing, or record forward references if symbols are undefined at the moment.
        exprs.zipWithIndex.foreach((tuple: (Expression, Int)) => {
            val storeValue = evaluateExpression(tuple._1) match {
                case Right(value) => value
                case Left(undefineds) =>
                    logger.debug("Symbol(s) (" + undefineds + ") are not yet defined on line " + line.number)
                    recordForwardReferences(undefineds, storage)
                    0
            }
            storage.data(tuple._2) = storeValue
        })

        validateDataSizes(line.number, storage.data, cellWidth)
        setDollar(getDollar + (cellWidth * exprs.size), line.number)

        storage
    }

    def allocateStorageForLine(line: Line, cellWidth: Int, count: Expression, repeatedExpr: Expression): Storage = {
        if (containsUndefineds(count)) {
            throw new AssemblyModelException("Count of '" + count + "' is undefined on line " + line.number)
        }
        val exprs = mutable.ArrayBuffer[Expression]()
        for (_ <- 0 until evaluateExpressionWithNoUndefineds(count)) {
            exprs += repeatedExpr
        }
        allocateStorageForLine(line, cellWidth, exprs.toList)
    }

    private def recordForwardReferences(undefinedSymbols: Set[String], storageToReEvaluate: Storage): Unit = {
        for (undefinedSymbol <- undefinedSymbols) {
            forwardReferenceFixups.getOrElseUpdate(undefinedSymbol, mutable.HashSet[Storage]()) += storageToReEvaluate
        }
    }

    private def resolveForwardReferences(symbolName: String, value: Int): Unit = {
        val storages = forwardReferences(symbolName)
        if (storages.nonEmpty) {
            logger.debug("Resolving references to symbol '" + symbolName + "' with value " + value)
            for (storage <- storages) {
                logger.debug("Resolving on line " + storage.line.number)

                // Re-evaluate expressions, storing, and removing the forward reference.
                storage.exprs.zipWithIndex.foreach((tuple: (Expression, Int)) => {
                    val storeValue = evaluateExpression(tuple._1) match {
                        case Right(result) => result
                        case Left(_) => 0 // Still undefined. More re-resolution to do.. or not..
                    }
                    storage.data(tuple._2) = storeValue
                })

            }
            forwardReferenceFixups.remove(symbolName)
        }
    }

    def forwardReferences(symbol: String): Set[Storage] = {
        forwardReferenceFixups.getOrElse(symbol, Set.empty).toSet
    }

    def endPass1(): Unit = {

        if (forwardReferenceFixups.nonEmpty) {
            // If there are any undefined symbols, sort them alphabetically, and list them with the line numbers they're
            // referenced on (sorted numerically). e.g. (aardvark: #1; FNORD: #3, #4; foo: #5; zygote: #1)
            val undefinedSymbolNamesSorted = forwardReferenceFixups.keySet.toList.sortWith((a: String, b: String) => {
                a.compareToIgnoreCase(b) < 0
            })
            val allStorageNamesAndLineReferences = undefinedSymbolNamesSorted.map((usn: String) => {
                val storageSet = forwardReferenceFixups(usn)
                val storageLinesSorted = storageSet.map(_.line.number).toList.sorted
                val storageLineReferences = storageLinesSorted.map("#" + _).mkString(", ")
                val storageNameAndLineReferences = usn + ": " + storageLineReferences

                storageNameAndLineReferences
            })
            throw new AssemblyModelException("Forward references remain unresolved at end of Pass 1: (" +
              allStorageNamesAndLineReferences.mkString("; ") + ")")
        }
    }
}
