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

object Endianness extends Enumeration {
    val Little, Big = Value
}

sealed abstract class SourcedValue(val line: Line)
// Macro expansions have the same line number as their invocation; hence line number -> list[storage+]
case class Storage(address: Int, cellWidth: Int, data: Array[Int], override val line: Line, exprs: List[Expression]) extends SourcedValue(line)
// Constant and Variable assignments are recalled against the line that sets them to a particular value
case class AssignmentValue(data: Int, override val line: Line, isLabel: Boolean) extends SourcedValue(line)

case class SymbolTableEntry(name: String, value: Int)

object UnresolvableSymbolType extends Enumeration {
    val Variable, Constant = Value // cannot be a Label, as this is always set to $, which is always known
}

case class UnresolvableSymbol(line: Line, symbolType: UnresolvableSymbolType.Value, name: String, expr: Expression)

/*
 * A mutable structure holding the output of the CodeGenerator.
 */
class AssemblyModel(debugCodegen: Boolean) {
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

    // All incoming Lines (original-in-source and macro expansion lines) are appended here. Recall that macro expansion
    // lines will have the same line number as original-in-source lines.
    private val lines = mutable.ArrayBuffer[Line]()
    // SourcedValues has a reference to its Line, so when the map of Undefined forward references -> Set[Storage]
    // is scanned at the end of the codegen phase, each Storage can show the Line on which the forward reference is.
    private val sourcedValuesForLineNumbers = mutable.HashMap[Int, mutable.ArrayBuffer[SourcedValue]]() // indexed by line number
    // And it's a map, since it's likely to be sparsely populated (not every line generates Storage)

    // Forward references are only resolved for Storages and Constants.

    // Any forward references to Storages are noted here, and resolution done on definition of the forwardly-referenced
    // symbol (either a variable, constant, or label).
    private val storageForwardReferenceFixups = mutable.HashMap[String, mutable.HashSet[Storage]]()

    // Any forward references to Constants/Variables are noted here, and resolution done on definition of the
    // forwardly-referenced symbol (either a variable, constant, or label).
    private val symbolForwardReferenceFixups = mutable.HashMap[String, mutable.HashSet[UnresolvableSymbol]]()

    private var endSeen = false

    var endianness: Endianness.Value = Endianness.Big

    // Initialise $ without storing back reference to a Line, since there isn't one.
    setDollarSilently(0)

    def getDollar: Int = getVariable(dollar)
    def setDollar(n: Int, line: Line): Unit = {
        setVariable(dollar, n, line)
    }
    def setDollarSilently(n: Int): Unit = {
        // Set $ without storing back reference to a Line, since there isn't one.
        logger.debug("Variable $ (silently) = " + n)
        variables.put(dollar, Value(n, 0))
    }
    def incrementDollar(n: Int): Unit = {
        setDollarSilently(getDollar + n)
    }

    def getVariable(name: String): Int = {
        variables.get(name.toUpperCase) match {
            case Some(vr) => vr.value
            case None => throw new AssemblyModelException("Variable '" + name + "' has not been defined")
        }
    }
    def variable(name: String): Option[Int] = variables.get(name.toUpperCase) match {
        case Some(con) => Some(con.value)
        case None => None
    }

    def setVariable(oddcasename: String, n: Int, line: Line): Unit = {
        val name = oddcasename.toUpperCase
        constants.get(name) match {
            case Some(con) => throw new AssemblyModelException("Variable '" + name + "' cannot override existing constant; initially defined on line " + con.definitionLine)
            case None => // drop through
        }
        labels.get(name) match {
            case Some(label) => throw new AssemblyModelException("Variable '" + name + "' cannot override existing label; initially defined on line " + label.definitionLine)
            case None => // drop through
        }
        variables.put(name, Value(n, line.number))
        sourcedValuesForLineNumber(line.number) += AssignmentValue(n, line, isLabel = false)
        logger.debug("Variable " + name + " = " + n)
        resolveForwardReferences(name, n)
    }

    def getConstant(name: String): Int = {
        constants.get(name.toUpperCase) match {
            case Some(con) => con.value
            case None => throw new AssemblyModelException("Constant '" + name + "' has not been defined")
        }
    }
    def constant(name: String): Option[Int] = constants.get(name.toUpperCase) match {
        case Some(con) => Some(con.value)
        case None => None
    }
    def setConstant(oddcasename: String, n: Int, line: Line): Unit = {
        val name = oddcasename.toUpperCase
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
                constants.put(name, Value(n, line.number))
                sourcedValuesForLineNumber(line.number) += AssignmentValue(n, line, isLabel = false)
                logger.debug("Constant " + name + " = " + n)
                resolveForwardReferences(name, n)
        }
    }

    def getLabel(name: String): Int = {
        labels.get(name.toUpperCase) match {
            case Some(con) => con.value
            case None => throw new AssemblyModelException("Label '" + name + "' has not been defined")
        }
    }
    def label(name: String): Option[Int] = labels.get(name.toUpperCase) match {
        case Some(label) => Some(label.value)
        case None => None
    }
    def setLabel(oddcasename: String, n: Int, line: Line): Unit = {
        val name = oddcasename.toUpperCase
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
                labels.put(name, Value(n, line.number))
                sourcedValuesForLineNumber(line.number) += AssignmentValue(n, line, isLabel = true)
                logger.debug("Label " + name + " = " + n)
                resolveForwardReferences(name, n)
        }
    }

    def getSymbols: List[SymbolTableEntry] = {
        def toSTE(pair: (String, Value)): SymbolTableEntry = {
            SymbolTableEntry(pair._1, pair._2.value)
        }

        val labelList = labels.toList
        val constantList = constants.toList
        (labelList ++ constantList).map(toSTE)
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
    private def lookupValue(oddcasename: SymbolName): Int = {
        val name = oddcasename.toUpperCase
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

    def definedValue(oddcasename: SymbolName): Boolean = {
        val name = oddcasename.toUpperCase
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

    def addLine(line: Line): Unit = {
        lines += line
    }

    def getSourcedValuesForLineNumber(lineNumber: Int): List[SourcedValue] = {
        sourcedValuesForLineNumbers.getOrElse(lineNumber, ArrayBuffer[SourcedValue]()).toList
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

    private def expandCharacterExpressions(possCharacterExprs: List[Expression]): List[Expression] = {
        possCharacterExprs.flatMap((expr: Expression) => {
            expr match {
                case Characters(chars) =>
                    chars.map((char: Char) => Number(char.toInt)).toList
                case _ =>
                    List(expr)
            }
        })
    }

    def allocateStorageForLine(line: Line, cellWidth: Int, exprs: List[Expression]): Storage = {
        val lineNumber = line.number
        val sourcedValues = sourcedValuesForLineNumber(lineNumber)

        // The incoming exprs will need evaluating to numbers that are stored in the Storage's data field. Most
        // expressions are evaluated to a single number, but Characters are evaluated to multiple. So expand all
        // elements of a Characters expression to an individual Number.
        val characterExpandedExprs = expandCharacterExpressions(exprs)

        val storage = Storage(getDollar, cellWidth, Array.ofDim[Int](characterExpandedExprs.size), line, characterExpandedExprs)
        sourcedValues += storage

        // Evaluate expressions, storing, or record forward references if symbols are undefined at the moment.
        characterExpandedExprs.zipWithIndex.foreach((tuple: (Expression, Int)) => {
            val storeValue = evaluateExpression(tuple._1) match {
                case Right(value) => value
                case Left(undefineds) =>
                    if (debugCodegen) {
                        logger.info("Symbol(s) (" + undefineds + ") are not yet defined on line " + lineNumber)
                    }
                    recordStorageForwardReferences(undefineds, storage)
                    0
            }
            storage.data(tuple._2) = storeValue
        })

        validateDataSizes(lineNumber, storage.data, cellWidth)
        incrementDollar(cellWidth * characterExpandedExprs.size)

        storage
    }

    private def sourcedValuesForLineNumber(lineNumber: Int) = {
        sourcedValuesForLineNumbers.getOrElseUpdate(lineNumber, mutable.ArrayBuffer[SourcedValue]())
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

    // Note that this will give you the original-in-source and macro expansion Lines, and for each Storage,
    // back-references to its originating Line. (Which will be the Line of the first argument.)
    // Each Line can have more than one SourcedValue: e.g. a Label on the same line as a DB generates an AssignmentValue
    // and a Storage.
    def foreachLineSourcedValues(op: (Line, List[SourcedValue]) => Unit): Unit = {
        for (line <- lines) { // can have many macro expanded lines' storages for this line number
            val lineNumber = line.number
            val sourcedValues = sourcedValuesForLineNumbers.getOrElse(lineNumber, mutable.ArrayBuffer[SourcedValue]()).toList
            val sourcedValuesForThisLine = sourcedValues.filter((s: SourcedValue) => {s.line == line}) // NB: don't compare on line number!

            op(line, sourcedValuesForThisLine)

            // So: line is the original Line, sourcedValues(x).line is always the Line that caused it, could be the original
            // Line or a macro expansion from it
            // And: sourcedValues could be empty, if there's no sourced value for Line
        }
    }

    // Note, this does not get you the original-in-source Lines, only those Lines that have had Storage allocated.
    def foreachSourcedValue(op: (Int, List[SourcedValue]) => Unit): Unit = {
        val lineNumbers = sourcedValuesForLineNumbers.keySet.toList.sorted
        lineNumbers.foreach(num => {
            val sourcedValues = sourcedValuesForLineNumbers(num).toList

            op(num, sourcedValues)
        })
    }

    // unresolvableSymbolName is of type symbolType, has Expression unresolvableExpr which cannot be evaluated since
    // it has undefined symbols, undefinedSymbols; unresolvableSymbolName is declared on line line.
    // e.g. <line 5> A EQU B+C*2
    // where B and C are undefined. unresolvableSymbolName=A, unresolvableExpr=B+C*2, undefinedSymbols={B,C},
    // symbolType=Constant and line = (5, "A EQU B+C*2", None, Some(unresolvableExpr)) (etc.)
    def recordSymbolForwardReferences(undefinedSymbols: Set[String], unresolvableSymbolName: String, unresolvableExpr: Expression, line: Line, symbolType: UnresolvableSymbolType.Value): Unit = {
        val unresolvableSymbol = UnresolvableSymbol(line, symbolType, unresolvableSymbolName, unresolvableExpr)
        for (undefinedSymbol <- undefinedSymbols) {
            symbolForwardReferenceFixups.getOrElseUpdate(undefinedSymbol.toUpperCase, mutable.HashSet[UnresolvableSymbol]()) += unresolvableSymbol
        }
        logger.debug(symbolForwardReferenceFixups.toString())
    }

    private def recordStorageForwardReferences(undefinedSymbols: Set[String], storageToReEvaluate: Storage): Unit = {
        for (undefinedSymbol <- undefinedSymbols) {
            storageForwardReferenceFixups.getOrElseUpdate(undefinedSymbol.toUpperCase, mutable.HashSet[Storage]()) += storageToReEvaluate
        }
    }

    // The Symbol (Label/Variable/Constant) symbolName has been resolved to a value. Where it had been recorded as
    // needing fixing up in Storages or other Symbols, fix up, and if each fix up is complete, remove the record of it
    // needing fixing up.
    private def resolveForwardReferences(symbolName: String, value: Int): Unit = {
        // TODO mark the storage as having had a forward reference resolved, so the R can be shown in the listing
        // TODO can the two types of forward reference fixup be generalised?

        // Resolve forward references to Storages...
        val storages = storageForwardReferences(symbolName)
        if (storages.nonEmpty) {
            logger.debug("Resolving Storage references to symbol '" + symbolName + "' with value " + value)
            for (storage <- storages) {
                logger.debug("Resolving on line " + storage.line.number)

                // Re-evaluate expressions, storing, and removing the forward reference.
                storage.exprs.zipWithIndex.foreach((tuple: (Expression, Int)) => {
                    val storeValue = evaluateExpression(tuple._1) match {
                        case Right(result) => result
                        case Left(_) => 0 // Expr still contains other undefined symbols, so more resolution to do....
                    }
                    storage.data(tuple._2) = storeValue
                })

            }
            storageForwardReferenceFixups.remove(symbolName)
        }

        // Resolve forward references to UnresolvableSymbols...
        val unresolvableSymbolExprs = unresolvableSymbolForwardReferences(symbolName)
        if (unresolvableSymbolExprs.nonEmpty) {
            logger.debug("Resolving Symbol references to symbol '" + symbolName + "' with value " + value)
            for (unresolvableSymbolExpr <- unresolvableSymbolExprs) {
                logger.debug("Resolving " + unresolvableSymbolExpr.symbolType + " " + unresolvableSymbolExpr.name + " on line " + unresolvableSymbolExpr.line.number)

                // Re-evaluate expressions, setting variable or constant, and removing the forward reference.
                evaluateExpression(unresolvableSymbolExpr.expr) match {
                    case Right(result) =>
                        unresolvableSymbolExpr.symbolType match {
                            case UnresolvableSymbolType.Constant =>
                                setConstant(unresolvableSymbolExpr.name, result, unresolvableSymbolExpr.line)
                            case UnresolvableSymbolType.Variable =>
                                setVariable(unresolvableSymbolExpr.name, result, unresolvableSymbolExpr.line)
                        }
                    case Left(_) => // Expr still contains other undefined symbols, so more resolution to do....
                }
            }
            symbolForwardReferenceFixups.remove(symbolName)
        }
    }

    def storageForwardReferences(symbol: String): Set[Storage] = {
        storageForwardReferenceFixups.getOrElse(symbol.toUpperCase, Set.empty).toSet
    }

    def unresolvableSymbolForwardReferences(symbol: String): Set[UnresolvableSymbol] = {
        symbolForwardReferenceFixups.getOrElse(symbol.toUpperCase, Set.empty).toSet
    }

    def checkUnresolvedForwardReferences(): Unit = {
        // If there are any undefined symbols, sort them alphabetically, and list them with the line numbers they're
        // referenced on (sorted numerically). e.g. (aardvark: #1; FNORD: #3, #4; foo: #5; zygote: #1)
        if (storageForwardReferenceFixups.nonEmpty) {
            val undefinedSymbolNamesSorted = storageForwardReferenceFixups.keySet.toList.sortWith((a: String, b: String) => {
                a.compareToIgnoreCase(b) < 0
            })
            val allStorageNamesAndLineReferences = undefinedSymbolNamesSorted.map((usn: String) => {
                val storageSet = storageForwardReferenceFixups(usn)
                val storageLinesSorted = storageSet.map(_.line.number).toList.sorted
                val storageLineReferences = storageLinesSorted.map("#" + _).mkString(", ")
                val storageNameAndLineReferences = usn + ": " + storageLineReferences

                storageNameAndLineReferences
            })
            throw new AssemblyModelException("Storage forward references remain unresolved at end of Pass 1: (" +
              allStorageNamesAndLineReferences.mkString("; ") + ")")
        }

        if (symbolForwardReferenceFixups.nonEmpty) {
            val undefinedSymbolNamesSorted = symbolForwardReferenceFixups.keySet.toList.sortWith((a: String, b: String) => {
                a.compareToIgnoreCase(b) < 0
            })
            val allStorageNamesAndLineReferences = undefinedSymbolNamesSorted.map((usn: String) => {
                val unresolvableSymbols = symbolForwardReferenceFixups(usn)
                val unresolvableSymbolLinesSorted = unresolvableSymbols.map(_.line.number).toList.sorted
                val unresolvableSymbolLineReferences = unresolvableSymbolLinesSorted.map("#" + _).mkString(", ")
                val unresolvableSymbolNameAndLineReferences = usn + ": " + unresolvableSymbolLineReferences

                unresolvableSymbolNameAndLineReferences
            })
            throw new AssemblyModelException("Symbol forward references remain unresolved at end of Pass 1: (" +
              allStorageNamesAndLineReferences.mkString("; ") + ")")
        }
    }

    def endHasBeenSeen(): Unit = {
        endSeen = true
    }

    def hasEndBeenSeen: Boolean = endSeen

    private var lowStorageAddress = 0
    private var highStorageAddress = 0

    private def calculateBounds(): Unit = {
        if ((lowStorageAddress, highStorageAddress) == (0, 0)) {
            if (sourcedValuesForLineNumbers.nonEmpty) {
                lowStorageAddress = Int.MaxValue
                highStorageAddress = Int.MinValue
                for (sourcedValues <- sourcedValuesForLineNumbers.values) {
                    for (sourcedValue <- sourcedValues) {
                        sourcedValue match {
                            case storage: Storage =>
                                val start = storage.address
                                val end = start + (storage.cellWidth * storage.data.length) - 1
                                logger.debug("start " + start + " end " + end + " (" + (end - start + 1) + " byte(s))")
                                if (start < lowStorageAddress) {
                                    logger.debug("new low bound")
                                    lowStorageAddress = start
                                }
                                if (end > highStorageAddress) {
                                    logger.debug("new high bound")
                                    highStorageAddress = end
                                }
                            case _ => // do nothing
                        }
                    }
                }
            }
        }
    }

    def lowestStorageAddress: Int = {
        calculateBounds()
        lowStorageAddress
    }

    def highestStorageAddress: Int = {
        calculateBounds()
        highStorageAddress
    }
}
