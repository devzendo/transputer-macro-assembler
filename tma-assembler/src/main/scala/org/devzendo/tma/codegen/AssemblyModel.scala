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
case class AssignmentValue(data: Int, override val line: Line, symbolType: SymbolType.Value) extends SourcedValue(line)

case class SymbolTableEntry(name: String, value: Int)

object SymbolType extends Enumeration {
    // This used to be named 'UnresolvableSymbolType', but now is used to denote symbol types for resolved ones.
    // Before implementing Convergence, this could not be a Label, as this is always set to $, which was always known.
    // Now blocks of code can converge, due to variable-length direct instruction encodings, a label's value can
    // change - and we need to record whether a symbol is a label, for fixing up Storage.
    val Variable, Constant, Label = Value
}

case class UnresolvableSymbol(line: Line, symbolType: SymbolType.Value, name: String, expr: Expression)

class SymbolForwardReferenceFixupState {

    var resolutionCount: Int = 0
    val unresolvableSymbols: mutable.HashSet[UnresolvableSymbol] = mutable.HashSet[UnresolvableSymbol]()
    def += (us: UnresolvableSymbol): Unit = {
        unresolvableSymbols += us
    }
    def nonEmpty: Boolean = unresolvableSymbols.nonEmpty
    def resolve(): Unit = {
        resolutionCount += 1
    }

    override def toString: SymbolName = s"resolutions $resolutionCount: references: $unresolvableSymbols"
}

class SymbolForwardReferenceFixups {
    val logger: Logger = org.log4s.getLogger

    private val map = mutable.HashMap[String, SymbolForwardReferenceFixupState]()

    def dump(): Unit = {
        logger.debug(s"Symbol forward reference fixups: Size ${size()}")
        def logEntry(entry: (String, SymbolForwardReferenceFixupState)): Unit = {
            logger.debug(s"Undefined Symbol ${entry._1}; Resolution Count: ${entry._2.resolutionCount}")
            val references = entry._2.unresolvableSymbols
            references.foreach((us: UnresolvableSymbol) => {
                logger.debug(s"  Unresolvable ${us.symbolType} ${us.name} line number ${us.line.number}")
            })
        }
        map.foreach (logEntry)
    }

    def -= (us: UnresolvableSymbol): Unit = {
        logger.debug(s"Removing $us from all entries in the fixup map")
        map.foreach (_._2.unresolvableSymbols -= us)
    }

    def += (symbolNameUC: SymbolName, us: UnresolvableSymbol): Unit = {
        logger.debug(s"Adding $us as needing fixup when $symbolNameUC is defined in the fixup map")
        getOrElseCreate(symbolNameUC) += us
    }

    private def getOrElseCreate(undefinedSymbolUC: SymbolName): SymbolForwardReferenceFixupState = {
        map.getOrElseUpdate(undefinedSymbolUC, new SymbolForwardReferenceFixupState())
    }

    def resolve(symbolNameUC: SymbolName): Unit = {
        map.get(symbolNameUC) match {
            case Some(fixupState) => fixupState.resolve()
            case None =>
        }
    }

    def size(): Int = map.size

    def resolutionCount(symbolNameUC: SymbolName): Int = {
        map.get(symbolNameUC) match {
            case Some(fixupState) => fixupState.resolutionCount
            case None => 0
        }
    }

    def getUnresolvableSymbols(symbolNameUC: SymbolName): Set[UnresolvableSymbol] = {
        map.get(symbolNameUC) match {
            case Some(fixupState) => fixupState.unresolvableSymbols.toSet
            case None => Set.empty
        }
    }

    def allUnresolvedSymbolForwardReferences(): Map[String, SymbolForwardReferenceFixupState] = {
        def isUnresolved(mapEntry: (String, SymbolForwardReferenceFixupState)): Boolean = {
            mapEntry._2.resolutionCount == 0
        }
        val unresolved = map.filter(isUnresolved).toMap
        logger.debug("Unresolved symbols: " + unresolved)
        unresolved
    }
}

class StorageForwardReferenceFixups {
    val logger: Logger = org.log4s.getLogger

    private val map = mutable.HashMap[String, mutable.HashSet[Storage]]()
    private val resolutionCount = mutable.HashMap[String, Integer]()

    def dump(): Unit = {
        logger.debug(s"Storage forward reference fixups: Size ${map.size}")
        map.keySet.foreach ((symbol: String) => {
            logger.debug(s"  Undefined Symbol ${map(symbol)}; Resolution Count: ${resolutionCount(symbol)}")
        })
    }

    def getSymbolReferences(symbolUC: String): Set[Storage] = {
        map.getOrElse(symbolUC, Set.empty).toSet
    }

    def unresolvedStorages(): Map[String, Set[Storage]] = {
        val unresolveds = map.filter { case (symbol, storageSet) => resolutionCount(symbol) == 0 }
        // return deep immutable version
        unresolveds.map {case (symbol, storageSet) => (symbol, storageSet.toSet)}.toMap
    }

    def += (symbolUC: String, storageToReEvaluate: Storage): mutable.Set[Storage] = {
        logger.debug(s"Adding storage for symbol $symbolUC, set resolution count to 0")
        resolutionCount.put(symbolUC, 0)
        map.getOrElseUpdate(symbolUC, mutable.HashSet[Storage]()) += storageToReEvaluate
    }

    def resolve(symbol: String, symbolType: SymbolType.Value): Unit = {
        val symbolUC = symbol.toUpperCase()
        logger.debug(s"Resolving symbol $symbolUC as a $symbolType")
        // Undefined Constants and Labels can be initially defined, and redefined during Convergence; So can
        // Variables - but they are defined once, and redefinition is not allowed.
        symbolType match {
            case SymbolType.Variable =>
                map.remove(symbolUC)

            case _ =>
                // Constants and Labels track changes, so don't get removed.
        }
        val increment = resolutionCount.getOrElseUpdate(symbolUC, -1) + 1
        resolutionCount.put(symbolUC, increment)
        logger.debug(s"Resolved resolution count of $symbolUC is now " + resolutionCount.get(symbolUC))
    }
}

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

    // "Converge Mode" is used when converging sequences of DirectInstructions containing undefined symbols: label and
    // constant addresses may be adjusted in this state. If not in this state, then throw on reassignment.
    var convergeMode: Boolean = false

    case class Value(value: Int, symbolType: SymbolType.Value, definitionLine: Int)

    private val symbols = mutable.HashMap[String, Value]()

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
    private val storageForwardReferenceFixups = new StorageForwardReferenceFixups()

    // Any forward references to Constants/Variables are noted here, and resolution done on definition of the
    // forwardly-referenced symbol (either a variable, constant, or label).
    private val symbolForwardReferenceFixups = new SymbolForwardReferenceFixups()

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
        symbols.put(dollar, Value(n, SymbolType.Variable, 0))
    }
    def incrementDollar(n: Int): Unit = {
        setDollarSilently(getDollar + n)
    }

    def getVariable(name: String): Int = {
        getSymbolValue(SymbolType.Variable, name.toUpperCase)
    }
    def variable(name: String): Option[Int] = {
        maybeSymbol(SymbolType.Variable, name.toUpperCase)
    }
    def setVariable(oddcasename: String, n: Int, line: Line): Unit = {
        setVariableInternal(n, line, oddcasename.toUpperCase, SymbolType.Variable)
    }

    def getConstant(name: String): Int = {
        getSymbolValue(SymbolType.Constant, name.toUpperCase)
    }
    def constant(name: String): Option[Int] = {
        maybeSymbol(SymbolType.Constant, name.toUpperCase)
    }
    def setConstant(oddcasename: String, n: Int, line: Line): Unit = {
        setConstantOrLabelInternal(n, line, oddcasename.toUpperCase, SymbolType.Constant)
    }

    def getLabel(name: String): Int = {
        getSymbolValue(SymbolType.Label, name.toUpperCase)
    }
    def label(name: String): Option[Int] =  {
        maybeSymbol(SymbolType.Label, name.toUpperCase)
    }
    def setLabel(oddcasename: String, n: Int, line: Line): Unit = {
        setConstantOrLabelInternal(n, line, oddcasename.toUpperCase, SymbolType.Label)
    }

    private def setVariableInternal(n: Int, line: Line, ucSymbolName: SymbolName, symbolType: SymbolType.Value) = {
        symbols.get(ucSymbolName) match {
            case Some(Value(_, `symbolType`, _)) => // drop through to reassign
            case Some(sym) => throw new AssemblyModelException(symbolType + " '" + ucSymbolName + "' cannot override existing " + sym.symbolType.toString.toLowerCase + "; initially defined on line " + sym.definitionLine)
            case None => // drop through
        }
        storeSymbolInternal(n, line, ucSymbolName, symbolType)
    }

    private def setConstantOrLabelInternal(n: Int, line: Line, ucSymbolName: SymbolName, symbolType: SymbolType.Value) = {
        // Allow replacement...
        if (convergeMode && symbolExists(symbolType, ucSymbolName)) {
            symbols.remove(ucSymbolName)
        }
        symbols.get(ucSymbolName) match {
            case Some(sym) => throw new AssemblyModelException(symbolType + " '" + ucSymbolName + "' cannot override existing " + sym.symbolType.toString.toLowerCase + "; defined on line " + sym.definitionLine)
            case None => storeSymbolInternal(n, line, ucSymbolName, symbolType)
        }
    }

    private def storeSymbolInternal(n: Int, line: Line, ucSymbolName: SymbolName, symbolType: SymbolType.Value) = {
        symbols.put(ucSymbolName, Value(n, symbolType, line.number))
        sourcedValuesForLineNumber(line.number) += AssignmentValue(n, line, symbolType)
        if (debugCodegen) {
            logger.info(symbolType + " " + ucSymbolName + " = " + n)
        }
        resolveForwardReferences(ucSymbolName, n, symbolType)
    }

    private def maybeSymbol(requiredSymbolType: SymbolType.Value, ucSymbolName: SymbolName) = {
        symbols.get(ucSymbolName) match {
            case Some(sym) => if (sym.symbolType == requiredSymbolType) Some(sym.value) else None
            case None => None
        }
    }

    private def symbolExists(requiredSymbolType: SymbolType.Value, ucSymbolName: SymbolName): Boolean = {
        maybeSymbol(requiredSymbolType, ucSymbolName).isDefined
    }

    private def getSymbolValue(requiredSymbolType: SymbolType.Value, ucSymbolName: SymbolName) = {
        symbols.get(ucSymbolName) match {
            case Some(Value(value, `requiredSymbolType`, _)) => value
            case _ => throw new AssemblyModelException(requiredSymbolType + " '" + ucSymbolName + "' has not been defined")
        }
    }

    def getSymbols: List[SymbolTableEntry] = {
        def toSTE(pair: (String, Value)): SymbolTableEntry = {
            SymbolTableEntry(pair._1, pair._2.value)
        }

        val labelsAndConstantsList = symbols.toList.filter(
            (p: (String, Value)) => { p._2.symbolType == SymbolType.Label || p._2.symbolType == SymbolType.Constant })
        labelsAndConstantsList.map(toSTE)
    }

    def getConvergeMode: Boolean = convergeMode

    def setConvergeMode(newMode: Boolean): Unit = {
        convergeMode = newMode
    }

    /**
      * Evaluate an expression, returning Left(Set(undefined variable names)) or Right(value)
      *
      * @param expr some expression, of any complexity
      * @return undefined variable names, or the evaluated value.
      */
    def evaluateExpression(expr: Expression): Either[Set[String], Int] = {
        //logger.debug("Evaluating " + expr)
        val undefineds = findUndefineds(expr)
        if (undefineds.nonEmpty) {
            //logger.debug("Undefined symbols: " + undefineds)
            Left(undefineds)
        } else {
            val value = evaluateExpressionWithNoUndefineds(expr)
            //logger.debug("Evaluation of " + expr + " = " + value)
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
        def getFallback(symbolType: SymbolType.Value, key: String)(fallback: => Int): Int = {
            symbols.get(key) match {
                case Some(x) => if (x.symbolType == symbolType) x.value else fallback
                case None => fallback
            }
        }
        getFallback(SymbolType.Variable, name) (getFallback(SymbolType.Constant, name) (getFallback(SymbolType.Label, name) ({
            throw new IllegalStateException("Precondition violation: " + name + " is supposed to be present as a variable/constant/label")
        })))
    }

    def definedValue(oddcasename: SymbolName): Boolean = {
        val name = oddcasename.toUpperCase
        symbols.contains(name)
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
            //logger.debug("cellWidth " + cellWidth + "; max " + max + "; name " + name + "; data " + dUnsigned)
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
        dumpStorage(storage)
        incrementDollar(cellWidth * characterExpandedExprs.size)

        storage
    }

    private def dumpStorage(storage: Storage): Unit = {
        if (debugCodegen) {
            def widthToDx(width: Int) = width match {
                case 1 => "DB"
                case 2 => "DW"
                case 4 => "DD"
            }

            def data(width: Int, data: Array[Int]) = {
                val hexnumStrings = data.map((d: Int) => {
                    width match {
                        case 1 => HexDump.byte2hex(d.toByte)
                        case 2 => HexDump.short2hex(d.toShort)
                        case 4 => HexDump.int2hex(d)
                    }
                })
                hexnumStrings.mkString(" ")
            }
            // This diagnostic does not honour endianness.
            logger.info(s"Storage @ ${HexDump.int2hex(storage.address)} ${widthToDx(storage.cellWidth)} (${data(storage.cellWidth, storage.data)})")
        }
    }

    private def sourcedValuesForLineNumber(lineNumber: Int) = {
        sourcedValuesForLineNumbers.getOrElseUpdate(lineNumber, mutable.ArrayBuffer[SourcedValue]())
    }

    def clearSourcedValuesForLineNumber(lineNumber: Int): Unit = {
        if (debugCodegen) {
            logger.debug("Clearing sourced values for line number " + lineNumber)
        }
        sourcedValuesForLineNumbers.remove(lineNumber)
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

    def allocateInstructionStorageForLine(line: Line, opbytes: List[Int]): Storage = {
        val lineNumber = line.number
        val sourcedValues = sourcedValuesForLineNumber(lineNumber)

        val storage = Storage(getDollar, 1, opbytes.toArray, line, opbytes map { Number })
        sourcedValues += storage

        validateDataSizes(lineNumber, storage.data, 1) // they should be instruction bytes, so this shouldn't fail
        dumpStorage(storage)
        incrementDollar(opbytes.size)

        storage
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

    // Called when a Constant or Variable is defined with one or more currently-undefined symbols in its expression.
    // Records the Constant/Variable against each of those currently-undefined symbols, so that when they are defined,
    // the Constant/Variable can be evaluated.
    //
    // unresolvableSymbolName is of type unresolvableSymbolType, has Expression unresolvableExpr which cannot be
    // evaluated since it has undefined symbols, undefinedSymbols; unresolvableSymbolName is declared on line line.
    // e.g. <line 5> A EQU B+C*2
    // where B and C are undefined...
    // unresolvableSymbolName=A, unresolvableExpr=B+C*2, undefinedSymbols={B,C},
    // unresolvableSymbolType=Constant and line = (5, "A EQU B+C*2", None, Some(unresolvableExpr)) (etc.)
    def recordSymbolForwardReferences(undefinedSymbols: Set[String], unresolvableSymbolName: String,
                                      unresolvableExpr: Expression, line: Line, unresolvableSymbolType: SymbolType.Value): Unit = {
        val unresolvableSymbol = UnresolvableSymbol(line, unresolvableSymbolType, unresolvableSymbolName, unresolvableExpr)
        for (undefinedSymbol <- undefinedSymbols) {
            val undefinedSymbolUC = undefinedSymbol.toUpperCase
            if (debugCodegen) {
                logger.info(s"Recording symbol $unresolvableSymbolName's forward reference to $undefinedSymbolUC")
            }
            // For all {B, C} add a reference to UnresolvableSymbol A
            // But {B, C} might be referenced from a set of other UnresolvableSymbols e.g. {D, E}
            // So B -> {A, D, E}
            // and C -> {A, D, E}
            symbolForwardReferenceFixups += (undefinedSymbolUC, unresolvableSymbol)  // (B, A) and (C, A)
        }

        if (debugCodegen) {
            symbolForwardReferenceFixups.dump()
        }
    }

    private def recordStorageForwardReferences(undefinedSymbols: Set[String], storageToReEvaluate: Storage): Unit = {
        for (undefinedSymbol <- undefinedSymbols) {
            if (debugCodegen) {
                logger.info(s"Recording storage forward reference to $undefinedSymbol")
            }
            storageForwardReferenceFixups += (undefinedSymbol.toUpperCase, storageToReEvaluate)
        }
        if (debugCodegen) {
            storageForwardReferenceFixups.dump()
        }
    }

    // The Symbol (Label/Variable/Constant) symbolName has been resolved to a value. Where it had been recorded as
    // needing fixing up in Storages or other Symbols, fix up, and if each fix up is complete, remove the record of it
    // needing fixing up.
    private def resolveForwardReferences(symbolName: String, value: Int, symbolType: SymbolType.Value): Unit = {
        // TODO mark the storage as having had a forward reference resolved, so the R can be shown in the listing
        // TODO can the two types of forward reference fixup be generalised?

        // Resolve forward references to Storages...
        val storagesWithForwardReferences = storageForwardReferences(symbolName)
        if (storagesWithForwardReferences.nonEmpty) {
            if (debugCodegen) {
                logger.info("Resolving Storage references to symbol '" + symbolName + "' with value " + value)
            }
            for (storage <- storagesWithForwardReferences) {
                if (debugCodegen) {
                    logger.info("Resolving on line " + storage.line.number)
                }

                // Re-evaluate expressions, storing, and removing the forward reference.
                storage.exprs.zipWithIndex.foreach((tuple: (Expression, Int)) => {
                    val storeValue = evaluateExpression(tuple._1) match {
                        case Right(result) => result
                        case Left(_) => 0 // Expr still contains other undefined symbols, so more resolution to do....
                    }
                    storage.data(tuple._2) = storeValue
                })
                dumpStorage(storage)
            }
            storageForwardReferenceFixups.resolve(symbolName, symbolType)
        }

        // Resolve forward references to UnresolvableSymbols...
        val unresolvableSymbols = unresolvedSymbolForwardReferences(symbolName)
        if (unresolvableSymbols.nonEmpty) {
            if (debugCodegen) {
                logger.info("Resolving Symbol references to symbol '" + symbolName + "' with value " + value)
            }
            for (unresolvableSymbol <- unresolvableSymbols) {
                if (debugCodegen) {
                    logger.info("Resolving " + unresolvableSymbol.symbolType + " " +
                      unresolvableSymbol.name + " on line " + unresolvableSymbol.line.number)
                }

                // Re-evaluate expressions, setting variable or constant, and removing the forward reference if it's a
                // variable.
                evaluateExpression(unresolvableSymbol.expr) match {
                    case Right(result) =>
                        unresolvableSymbol.symbolType match {
                            case SymbolType.Constant =>
                                setConstant(unresolvableSymbol.name, result, unresolvableSymbol.line)
                                // When Converging, constant changes are tracked, so keep the
                                // unresolvableSymbolReference in the fixup map.
                                if (debugCodegen) {
                                    logger.debug(s"${unresolvableSymbol.name} is a Constant so not removing from fixup map, so convergence will track changes")
                                }
                            case SymbolType.Variable =>
                                setVariable(unresolvableSymbol.name, result, unresolvableSymbol.line)
                                // When Converging, variable changes are not tracked, so remove the
                                // unresolvableSymbolReference in the fixup map, now that it has been set initially.
                                if (debugCodegen) {
                                    logger.debug(s"${unresolvableSymbol.name} is a Variable and not in Converge mode,so removing from fixup map")
                                }
                                symbolForwardReferenceFixups -= unresolvableSymbol
                            case SymbolType.Label => // This should not happen....
                                throw new AssemblyModelException("Labels cannot be unresolvable?")
                        }
                    case Left(_) => // Expr still contains other undefined symbols, so more resolution to do....
                }
                symbolForwardReferenceFixups.resolve(symbolName)
                symbolForwardReferenceFixups.dump()
            }
        }
    }

    private [codegen] def storageForwardReferences(symbol: String): Set[Storage] = {
        storageForwardReferenceFixups.getSymbolReferences(symbol.toUpperCase())
    }

    private [codegen] def unresolvedSymbolForwardReferences(symbol: String): Set[UnresolvableSymbol] = {
        symbolForwardReferenceFixups.getUnresolvableSymbols(symbol.toUpperCase)
    }

    private [codegen] def resolutionCount(symbol: SymbolName): Int = {
        symbolForwardReferenceFixups.resolutionCount(symbol.toUpperCase)
    }


    def checkUnresolvedForwardReferences(): Unit = {
        // If there are any undefined symbols, sort them alphabetically, and list them with the line numbers they're
        // referenced on (sorted numerically). e.g. (aardvark: #1; FNORD: #3, #4; foo: #5; zygote: #1)
        val unresolvedStorages: Map[String, Set[Storage]] = storageForwardReferenceFixups.unresolvedStorages()
        if (unresolvedStorages.nonEmpty) {
            val undefinedSymbolNamesSorted = unresolvedStorages.keySet.toList.sortWith((a: String, b: String) => {
                a.compareToIgnoreCase(b) < 0
            })
            val allStorageNamesAndLineReferences = undefinedSymbolNamesSorted.map((usn: String) => {
                val storageSet = unresolvedStorages(usn)
                val storageLinesSorted = storageSet.map(_.line.number).toList.sorted
                val storageLineReferences = storageLinesSorted.map("#" + _).mkString(", ")
                val storageNameAndLineReferences = usn + ": " + storageLineReferences

                storageNameAndLineReferences
            })
            throw new AssemblyModelException("Storage forward references remain unresolved at end of Pass 1: (" +
              allStorageNamesAndLineReferences.mkString("; ") + ")")
        }


        val unresolvedSymbols = symbolForwardReferenceFixups.allUnresolvedSymbolForwardReferences()
        if (unresolvedSymbols.nonEmpty) {
            val undefinedSymbolNamesSorted = unresolvedSymbols.keySet.toList.sortWith((a: String, b: String) => {
                a.compareToIgnoreCase(b) < 0
            })
            val allStorageNamesAndLineReferences = undefinedSymbolNamesSorted.map((usn: String) => {
                val unresolvableSymbols = unresolvedSymbols(usn).unresolvableSymbols
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

    def dump(): Unit = {
        foreachLineSourcedValues((line: Line, sourcedValues: List[SourcedValue]) => {
            logger.debug("----------")
            logger.debug(s"line $line")
            for (sv <- sourcedValues) {
                sv match {
                    case st: Storage =>
                        logger.debug(s"  storage data ${st.data.toList} address ${st.address}")
                    case av: AssignmentValue =>
                        logger.debug(s"  assigned value ${av.data}")
                }
            }
            logger.debug("----------")
        })
    }
}
