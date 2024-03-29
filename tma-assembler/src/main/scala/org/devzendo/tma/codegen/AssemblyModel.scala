/*
 * Copyright (C) 2008-2023 Matt Gumbley, DevZendo.org http://devzendo.org
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

sealed abstract class SourcedValue(val indexedLine: IndexedLine)
// Macro expansions have the same line number as their invocation; hence line number -> list[storage+]
case class Storage(address: Int, cellWidth: Int, data: Array[Int], override val indexedLine: IndexedLine, exprs: List[Expression]) extends SourcedValue(indexedLine)
// Constant and Variable assignments are recalled against the line that sets them to a particular value
case class AssignmentValue(data: Int, override val indexedLine: IndexedLine, symbolType: SymbolType.Value) extends SourcedValue(indexedLine)

case class SymbolTableEntry(casedSymbolName: CasedSymbolName, value: Int)

object SymbolType extends Enumeration {
    // This used to be named 'UnresolvableSymbolType', but now is used to denote symbol types for resolved ones.
    // Before implementing Convergence, this could not be a Label, as this is always set to $, which was always known.
    // Now blocks of code can converge, due to variable-length direct instruction encodings, a label's value can
    // change - and we need to record whether a symbol is a label, for fixing up Storage.
    val Variable, Constant, Label = Value
}

case class UnresolvableSymbol(indexedLine: IndexedLine, symbolType: SymbolType.Value, casedSymbolName: CasedSymbolName, expr: Expression)

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

    private val map = mutable.HashMap[CasedSymbolName, SymbolForwardReferenceFixupState]()

    def dump(): Unit = {
        logger.debug(s"Symbol forward reference fixups: Size ${size()}")
        def logEntry(entry: (CasedSymbolName, SymbolForwardReferenceFixupState)): Unit = {
            logger.debug(s"Undefined Symbol ${entry._1}; Resolution Count: ${entry._2.resolutionCount}")
            val references = entry._2.unresolvableSymbols
            references.foreach((us: UnresolvableSymbol) => {
                logger.debug(s"  Unresolvable ${us.symbolType} ${us.casedSymbolName} line number ${us.indexedLine.location.lineNumber}")
            })
        }
        map.foreach (logEntry)
    }

    def -= (us: UnresolvableSymbol): Unit = {
        logger.debug(s"Removing $us from all entries in the fixup map")
        map.foreach (_._2.unresolvableSymbols -= us)
    }

    def += (casedSymbolName: CasedSymbolName, us: UnresolvableSymbol): Unit = {
        logger.debug(s"Adding $us as needing fixup when $casedSymbolName is defined in the fixup map")
        getOrElseCreate(casedSymbolName) += us
    }

    private def getOrElseCreate(casedSymbolName: CasedSymbolName): SymbolForwardReferenceFixupState = {
        map.getOrElseUpdate(casedSymbolName, new SymbolForwardReferenceFixupState())
    }

    def resolve(casedSymbolName: CasedSymbolName): Unit = {
        map.get(casedSymbolName) match {
            case Some(fixupState) => fixupState.resolve()
            case None =>
        }
    }

    def size(): Int = map.size

    def resolutionCount(casedSymbolName: CasedSymbolName): Int = {
        map.get(casedSymbolName) match {
            case Some(fixupState) => fixupState.resolutionCount
            case None => 0
        }
    }

    def getUnresolvableSymbols(casedSymbolName: CasedSymbolName): Set[UnresolvableSymbol] = {
        map.get(casedSymbolName) match {
            case Some(fixupState) => fixupState.unresolvableSymbols.toSet
            case None => Set.empty
        }
    }

    def allUnresolvedSymbolForwardReferences(): Map[CasedSymbolName, SymbolForwardReferenceFixupState] = {
        def isUnresolved(mapEntry: (CasedSymbolName, SymbolForwardReferenceFixupState)): Boolean = {
            mapEntry._2.resolutionCount == 0
        }
        val unresolved = map.filter(isUnresolved).toMap
        logger.debug("Unresolved symbols: " + unresolved)
        unresolved
    }
}

class StorageForwardReferenceFixups {
    val logger: Logger = org.log4s.getLogger

    private val map = mutable.HashMap[CasedSymbolName, mutable.HashSet[Storage]]()
    private val resolutionCount = mutable.HashMap[CasedSymbolName, Integer]()

    def dump(): Unit = {
        logger.debug(s"Storage forward reference fixups: Size ${map.size}")
        map.keySet.foreach ((casedSymbolName: CasedSymbolName) => {
            logger.debug(s"  Undefined Symbol ${map(casedSymbolName)}; Resolution Count: ${resolutionCount(casedSymbolName)}")
        })
    }

    def getSymbolReferences(casedSymbolName: CasedSymbolName): Set[Storage] = {
        map.getOrElse(casedSymbolName, Set.empty).toSet
    }

    def unresolvedStorages(): Map[CasedSymbolName, Set[Storage]] = {
        val unresolveds = map.filter { case (symbol, _) => resolutionCount(symbol) == 0 }
        // return deep immutable version
        unresolveds.map {case (symbol, storageSet) => (symbol, storageSet.toSet)}.toMap
    }

    def += (casedSymbolName: CasedSymbolName, storageToReEvaluate: Storage): mutable.Set[Storage] = {
        logger.debug(s"Adding storage for symbol $casedSymbolName, set resolution count to 0")
        resolutionCount.put(casedSymbolName, 0)
        map.getOrElseUpdate(casedSymbolName, mutable.HashSet[Storage]()) += storageToReEvaluate
    }

    def resolve(casedSymbolName: CasedSymbolName, symbolType: SymbolType.Value): Unit = {
        logger.debug(s"Resolving symbol $casedSymbolName as a $symbolType")
        // Undefined Constants and Labels can be initially defined, and redefined during Convergence; So can
        // Variables - but they are defined once, and redefinition is not allowed.
        symbolType match {
            case SymbolType.Variable =>
                map.remove(casedSymbolName)

            case _ =>
                // Constants and Labels track changes, so don't get removed.
        }
        val increment = resolutionCount.getOrElseUpdate(casedSymbolName, -1) + 1
        resolutionCount.put(casedSymbolName, increment)
        logger.debug(s"Resolved resolution count of $casedSymbolName is now " + resolutionCount.get(casedSymbolName))
    }
}

/*
 * A mutable structure holding the output of the CodeGenerator.
 */
class AssemblyModel(debugCodegen: Boolean) {
    val logger: Logger = org.log4s.getLogger

    private val dollar = CasedSymbolName("$")

    var title = ""
    var rows = 25
    var columns = 80
    var processor: Option[String] = None

    // "Converge Mode" is used when converging sequences of DirectInstructions containing undefined symbols: label and
    // constant addresses may be adjusted in this state. If not in this state, then throw on reassignment.
    var convergeMode: Boolean = false

    case class Value(value: Int, symbolType: SymbolType.Value, definitionLine: Int)

    private val symbols = mutable.HashMap[CasedSymbolName, Value]()

    // All incoming Lines (original-in-source and macro expansion lines) are appended here after they have been
    // transformed by any StatementTransformers. Recall that macro expansion lines will have the same line number as
    // original-in-source lines.
    private val lines = mutable.ArrayBuffer[IndexedLine]()
    // SourcedValues has a reference to its Line, so when the map of Undefined forward references -> Set[Storage]
    // is scanned at the end of the codegen phase, each Storage can show the Line on which the forward reference is.
    private val sourcedValuesForLineIndices = mutable.HashMap[Int, mutable.ArrayBuffer[SourcedValue]]() // indexed by line number
    // And it's a map, since it's likely to be sparsely populated (not every line generates Storage)
    // Recall that macro expansions could lead to multiple entries here with the same line number, hence the ArrayBuffer.

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
    def setDollar(n: Int, indexedLine: IndexedLine): Unit = {
        setVariable(dollar, n, indexedLine)
    }
    def setDollarSilently(n: Int): Unit = {
        // Set $ without storing back reference to a Line, since there isn't one.
        logger.debug("Variable $ (silently) = 0x" + HexDump.int2hex(n))
        symbols.put(dollar, Value(n, SymbolType.Variable, 0))
    }
    def incrementDollar(n: Int): Unit = {
        setDollarSilently(getDollar + n)
    }

    def getVariable(casedSymbolName: CasedSymbolName): Int = {
        getSymbolValue(SymbolType.Variable, casedSymbolName)
    }
    def variable(casedSymbolName: CasedSymbolName): Option[Int] = {
        maybeSymbol(SymbolType.Variable, casedSymbolName)
    }
    def setVariable(casedSymbolName: CasedSymbolName, n: Int, indexedLine: IndexedLine): Unit = {
        setVariableInternal(n, indexedLine, casedSymbolName, SymbolType.Variable)
    }

    // For use during convergence...
    def getVariables(): Map[CasedSymbolName, Int] = {
        def toNamedInt(pair: (CasedSymbolName, Value)): (CasedSymbolName, Int) = {
            (pair._1, pair._2.value)
        }
        val variablesList = symbols.toList.filter(
            (p: (CasedSymbolName, Value)) => {
                p._2.symbolType == SymbolType.Variable
            })
        variablesList.map(toNamedInt).toMap
    }

    def resetVariablesSilently(vars: Map[CasedSymbolName, Int]): Unit = {
        def resetVariable(pair: (CasedSymbolName, Int)): Unit = {
            // Like storeSymbolInternal but with a silent diagnostic. Used in convergence where storage will be cleared
            // at the start of each iteration.
            val orig = symbols.get(pair._1).get
            symbols.put(pair._1, Value(pair._2, SymbolType.Variable, orig.definitionLine))
            // sourcedValuesArrayBufferForLineIndex(indexedLine.lineIndex) += AssignmentValue(n, indexedLine, symbolType)
            if (debugCodegen) {
                logger.debug("Variable " + pair._1 + " (silently) = 0x" + HexDump.int2hex(pair._2))
            }
            //resolveForwardReferences(casedSymbolName, n, symbolType)
        }
        vars.foreach(resetVariable)
    }

    def getConstant(casedSymbolName: CasedSymbolName): Int = {
        getSymbolValue(SymbolType.Constant, casedSymbolName)
    }
    def constant(casedSymbolName: CasedSymbolName): Option[Int] = {
        maybeSymbol(SymbolType.Constant, casedSymbolName)
    }
    def setConstant(casedSymbolName: CasedSymbolName, n: Int, indexedLine: IndexedLine): Unit = {
        setConstantOrLabelInternal(n, indexedLine, casedSymbolName, SymbolType.Constant)
    }

    def getLabel(casedSymbolName: CasedSymbolName): Int = {
        getSymbolValue(SymbolType.Label, casedSymbolName)
    }
    def label(casedSymbolName: CasedSymbolName): Option[Int] =  {
        maybeSymbol(SymbolType.Label, casedSymbolName)
    }
    def setLabel(casedSymbolName: CasedSymbolName, n: Int, indexedLine: IndexedLine): Unit = {
        setConstantOrLabelInternal(n, indexedLine, casedSymbolName, SymbolType.Label)
    }

    private def setVariableInternal(n: Int, indexedLine: IndexedLine, casedSymbolName: CasedSymbolName, symbolType: SymbolType.Value): Unit = {
        symbols.get(casedSymbolName) match {
            case Some(Value(_, `symbolType`, _)) => // drop through to reassign
            case Some(sym) => throw new AssemblyModelException(symbolType + " '" + casedSymbolName + "' cannot override existing " + sym.symbolType.toString.toLowerCase + "; initially defined on line " + sym.definitionLine)
            case None => // drop through
        }
        storeSymbolInternal(n, indexedLine, casedSymbolName, symbolType)
    }

    private def setConstantOrLabelInternal(n: Int, indexedLine: IndexedLine, casedSymbolName: CasedSymbolName, symbolType: SymbolType.Value): Unit = {
        // Allow replacement...
        if (convergeMode && symbolExists(symbolType, casedSymbolName))
            symbols.remove(casedSymbolName)
        symbols.get(casedSymbolName) match {
            case Some(sym) => throw new AssemblyModelException(symbolType + " '" + casedSymbolName + "' cannot override existing " + sym.symbolType.toString.toLowerCase + "; defined on line " + sym.definitionLine)
            case None => storeSymbolInternal(n, indexedLine, casedSymbolName, symbolType)
        }
    }

    private def storeSymbolInternal(n: Int, indexedLine: IndexedLine, casedSymbolName: CasedSymbolName, symbolType: SymbolType.Value): Unit = {
        symbols.put(casedSymbolName, Value(n, symbolType, indexedLine.location.lineNumber))
        sourcedValuesArrayBufferForLineIndex(indexedLine.lineIndex) += AssignmentValue(n, indexedLine, symbolType)
        if (debugCodegen) {
            logger.info(symbolType + " " + casedSymbolName + " = 0x" + HexDump.int2hex(n))
        }
        resolveForwardReferences(casedSymbolName, n, symbolType)
    }

    private def maybeSymbol(requiredSymbolType: SymbolType.Value, casedSymbolName: CasedSymbolName) = {
        symbols.get(casedSymbolName) match {
            case Some(sym) => if (sym.symbolType == requiredSymbolType) Some(sym.value) else None
            case None => None
        }
    }

    private def symbolExists(requiredSymbolType: SymbolType.Value, casedSymbolName: CasedSymbolName): Boolean = {
        maybeSymbol(requiredSymbolType, casedSymbolName).isDefined
    }

    private def getSymbolValue(requiredSymbolType: SymbolType.Value, casedSymbolName: CasedSymbolName) = {
        symbols.get(casedSymbolName) match {
            case Some(Value(value, `requiredSymbolType`, _)) => value
            case _ => throw new AssemblyModelException(requiredSymbolType + " '" + casedSymbolName + "' has not been defined")
        }
    }

    def getLabelsAndConstants: List[SymbolTableEntry] = {
        def toSTE(pair: (CasedSymbolName, Value)): SymbolTableEntry = {
            SymbolTableEntry(pair._1, pair._2.value)
        }

        val labelsAndConstantsList = symbols.toList.filter(
            (p: (CasedSymbolName, Value)) => { p._2.symbolType == SymbolType.Label || p._2.symbolType == SymbolType.Constant })
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
    def evaluateExpression(expr: Expression): Either[Set[CasedSymbolName], Int] = {
        logger.debug("Evaluating " + expr)
        val undefineds = findUndefineds(expr)
        if (undefineds.nonEmpty) {
            logger.debug("Undefined symbols: " + undefineds)
            Left(undefineds)
        } else {
            val value = evaluateExpressionWithNoUndefineds(expr)
            logger.debug("Evaluation of " + expr + " = 0x" + HexDump.int2hex(value))
            Right(value)
        }
    }

    // precondition: all SymbolArgs here are defined as a variable/constant/label
    private def evaluateExpressionWithNoUndefineds(expr: Expression): Int = {
        expr match {
            case SymbolArg(name) => lookupValue(CasedSymbolName(name))
            case Number(n) => n
            case Characters(_) => throw new AssemblyModelException("Cannot evaluate '" + expr + "' as an Int")
            case Unary(op, uExpr) => evaluateUnary(op, uExpr)
            case Binary(op, lExpr, rExpr) => evaluateBinary(op, lExpr, rExpr)
        }
    }

    // precondition: name is defined as a variable/constant/label
    private def lookupValue(name: CasedSymbolName): Int = {
        def getFallback(symbolType: SymbolType.Value, key: CasedSymbolName)(fallback: => Int): Int = {
            symbols.get(key) match {
                case Some(x) => if (x.symbolType == symbolType) x.value else fallback
                case None => fallback
            }
        }
        getFallback(SymbolType.Variable, name) (getFallback(SymbolType.Constant, name) (getFallback(SymbolType.Label, name) ({
            throw new IllegalStateException("Precondition violation: " + name + " is supposed to be present as a variable/constant/label")
        })))
    }

    def definedValue(casedSymbolName: CasedSymbolName): Boolean = {
        symbols.contains(casedSymbolName)
    }

    def findUndefineds(expr: Expression): Set[CasedSymbolName] = {
        expr match {
            case SymbolArg(name) => if (definedValue(CasedSymbolName(name))) Set.empty else Set(CasedSymbolName(name))
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
            case OffsetFrom(storedDollar) =>
                val ret = value - storedDollar
                logger.debug("Offset of value=%d (0x%x) and stored $=%d (0x%x): %d (0x%x)".format(value, value, storedDollar, storedDollar, ret, ret))
                ret
            case Offset() =>
                throw new IllegalStateException("Offset should have been transformed to an OffsetFrom")
            case _ =>
                throw new IllegalStateException("Parser has passed an operation of " + op + " to a Unary")
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

    def addLine(indexedLine: IndexedLine): Unit = {
        lines += indexedLine
    }

    def replaceLine(lineIndex: Int, indexedLine: IndexedLine): Unit = {
        lines.update(lineIndex, indexedLine)
    }

    def getSourcedValuesForLineIndex(lineIndex: Int): List[SourcedValue] = {
        sourcedValuesForLineIndices.getOrElse(lineIndex, ArrayBuffer[SourcedValue]()).toList
    }

    private def sourcedValuesArrayBufferForLineIndex(lineIndex: Int) = {
        sourcedValuesForLineIndices.getOrElseUpdate(lineIndex, mutable.ArrayBuffer[SourcedValue]())
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

    def allocateStorageForLine(indexedLine: IndexedLine, cellWidth: Int, exprs: List[Expression]): Storage = {
        val lineNumber = indexedLine.location.lineNumber
        val existingSourcedValues = sourcedValuesArrayBufferForLineIndex(indexedLine.lineIndex)
        // The incoming exprs will need evaluating to numbers that are stored in the Storage's data field. Most
        // expressions are evaluated to a single number, but Characters are evaluated to multiple. So expand all
        // elements of a Characters expression to an individual Number.
        val characterExpandedExprs = expandCharacterExpressions(exprs)

        val storage = Storage(getDollar, cellWidth, Array.ofDim[Int](characterExpandedExprs.size), indexedLine, characterExpandedExprs)
        existingSourcedValues += storage

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
            logger.info(s"Storage @ 0x${HexDump.int2hex(storage.address)} ${widthToDx(storage.cellWidth)} (${data(storage.cellWidth, storage.data)})")
        }
    }

    def clearSourcedValuesForLineIndex(lineIndex: Int): Unit = {
        if (debugCodegen) {
            logger.debug("Clearing sourced values for line index " + lineIndex)
        }
        sourcedValuesForLineIndices.remove(lineIndex)
    }

    def allocateInstructionStorageForLine(indexedLine: IndexedLine, opbytes: List[Int]): Storage = {
        val lineNumber = indexedLine.location.lineNumber
        val existingSourcedValues = sourcedValuesArrayBufferForLineIndex(indexedLine.lineIndex)

        val storage = Storage(getDollar, 1, opbytes.toArray, indexedLine, opbytes map { Number })
        existingSourcedValues += storage

        validateDataSizes(lineNumber, storage.data, 1) // they should be instruction bytes, so this shouldn't fail
        dumpStorage(storage)
        incrementDollar(opbytes.size)

        storage
    }

    // Note that this will give you the original-in-source and macro expansion Lines, and for each Storage,
    // back-references to its originating Line. (Which will be the Line of the first argument.)
    // Each Line can have more than one SourcedValue: e.g. a Label on the same line as a DB generates an AssignmentValue
    // and a Storage.
    def foreachLineSourcedValues(op: (IndexedLine, List[SourcedValue]) => Unit): Unit = {
        lines.foreach { indexedLine: IndexedLine =>
            // NB: can have many macro expanded lines' storages for each line number
            val sourcedValues = getSourcedValuesForLineIndex(indexedLine.lineIndex)
            val sourcedValuesForThisLine = sourcedValues.filter((s: SourcedValue) => {s.indexedLine == indexedLine}) // NB: don't compare on line number!

            op(indexedLine, sourcedValuesForThisLine)

            // So: indexedLine is the original Line, sourcedValues(x).indexedLine is always the IndexedLine that caused
            // it, could be the original line or a macro expansion from it
            // And: sourcedValues could be empty, if there's no sourced value for IndexedLine
        }
    }

    def allLines(): List[IndexedLine] = {
        lines.toList
    }

    // Note, this does not get you the original-in-source IndexedLines, only those Lines that have had Storage allocated.
    def foreachSourcedValue(op: (Int, List[SourcedValue]) => Unit): Unit = {
        val lineIndices = sourcedValuesForLineIndices.keySet.toList.sorted
        lineIndices.foreach(num => {
            val sourcedValues = getSourcedValuesForLineIndex(num)

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
    def recordSymbolForwardReferences(undefinedSymbols: Set[CasedSymbolName], unresolvableSymbolName: CasedSymbolName,
                                      unresolvableExpr: Expression, indexedLine: IndexedLine,
                                      unresolvableSymbolType: SymbolType.Value): Unit = {
        val unresolvableSymbol = UnresolvableSymbol(indexedLine, unresolvableSymbolType, unresolvableSymbolName, unresolvableExpr)
        for (undefinedSymbol <- undefinedSymbols) {
            if (debugCodegen) {
                logger.info(s"Recording symbol $unresolvableSymbolName's forward reference to $undefinedSymbol")
            }
            // For all {B, C} add a reference to UnresolvableSymbol A
            // But {B, C} might be referenced from a set of other UnresolvableSymbols e.g. {D, E}
            // So B -> {A, D, E}
            // and C -> {A, D, E}
            symbolForwardReferenceFixups += (undefinedSymbol, unresolvableSymbol)  // (B, A) and (C, A)
        }

        if (debugCodegen) {
            symbolForwardReferenceFixups.dump()
        }
    }

    private def recordStorageForwardReferences(undefinedSymbols: Set[CasedSymbolName], storageToReEvaluate: Storage): Unit = {
        for (undefinedSymbol <- undefinedSymbols) {
            if (debugCodegen) {
                logger.info(s"Recording storage forward reference to $undefinedSymbol")
            }
            storageForwardReferenceFixups += (undefinedSymbol, storageToReEvaluate)
        }
        if (debugCodegen) {
            storageForwardReferenceFixups.dump()
        }
    }

    // The Symbol (Label/Variable/Constant) symbolName has been resolved to a value. Where it had been recorded as
    // needing fixing up in Storages or other Symbols, fix up, and if each fix up is complete, remove the record of it
    // needing fixing up.
    private def resolveForwardReferences(casedSymbolName: CasedSymbolName, value: Int, symbolType: SymbolType.Value): Unit = {
        // TODO mark the storage as having had a forward reference resolved, so the R can be shown in the listing
        // TODO can the two types of forward reference fixup be generalised?

        // Resolve forward references to Storages...
        val storagesWithForwardReferences = storageForwardReferences(casedSymbolName)
        if (storagesWithForwardReferences.nonEmpty) {
            if (debugCodegen) {
                logger.info("Resolving Storage references to symbol '" + casedSymbolName + "' with value " + value)
            }
            for (storage <- storagesWithForwardReferences) {
                if (debugCodegen) {
                    logger.info("Resolving on line " + storage.indexedLine.location.lineNumber)
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
            storageForwardReferenceFixups.resolve(casedSymbolName, symbolType)
        }

        // Resolve forward references to UnresolvableSymbols...
        val unresolvableSymbols = unresolvedSymbolForwardReferences(casedSymbolName)
        if (unresolvableSymbols.nonEmpty) {
            if (debugCodegen) {
                logger.info("Resolving Symbol references to symbol '" + casedSymbolName + "' with value " + value)
            }
            for (unresolvableSymbol <- unresolvableSymbols) {
                if (debugCodegen) {
                    logger.info("Resolving " + unresolvableSymbol.symbolType + " " +
                      unresolvableSymbol.casedSymbolName + " on line " + unresolvableSymbol.indexedLine.location.lineNumber)
                }

                // Re-evaluate expressions, setting variable or constant, and removing the forward reference if it's a
                // variable.
                evaluateExpression(unresolvableSymbol.expr) match {
                    case Right(result) =>
                        unresolvableSymbol.symbolType match {
                            case SymbolType.Constant =>
                                setConstant(unresolvableSymbol.casedSymbolName, result, unresolvableSymbol.indexedLine)
                                // When Converging, constant changes are tracked, so keep the
                                // unresolvableSymbolReference in the fixup map.
                                if (debugCodegen) {
                                    logger.debug(s"${unresolvableSymbol.casedSymbolName} is a Constant so not removing from fixup map, so convergence will track changes")
                                }
                            case SymbolType.Variable =>
                                setVariable(unresolvableSymbol.casedSymbolName, result, unresolvableSymbol.indexedLine)
                                // When Converging, variable changes are not tracked, so remove the
                                // unresolvableSymbolReference in the fixup map, now that it has been set initially.
                                if (debugCodegen) {
                                    logger.debug(s"${unresolvableSymbol.casedSymbolName} is a Variable and not in Converge mode, so removing from fixup map")
                                }
                                symbolForwardReferenceFixups -= unresolvableSymbol
                            case SymbolType.Label => // This should not happen....
                                throw new AssemblyModelException("Labels cannot be unresolvable?")
                        }
                    case Left(_) => // Expr still contains other undefined symbols, so more resolution to do....
                }
                symbolForwardReferenceFixups.resolve(casedSymbolName)
                symbolForwardReferenceFixups.dump()
            }
        }
    }

    private [codegen] def storageForwardReferences(casedSymbolName: CasedSymbolName): Set[Storage] = {
        storageForwardReferenceFixups.getSymbolReferences(casedSymbolName)
    }

    private [codegen] def unresolvedSymbolForwardReferences(casedSymbolName: CasedSymbolName): Set[UnresolvableSymbol] = {
        symbolForwardReferenceFixups.getUnresolvableSymbols(casedSymbolName)
    }

    private [codegen] def resolutionCount(casedSymbolName: CasedSymbolName): Int = {
        symbolForwardReferenceFixups.resolutionCount(casedSymbolName: CasedSymbolName)
    }


    def checkUnresolvedForwardReferences(): Unit = {
        // If there are any undefined symbols, sort them alphabetically, and list them with the line numbers they're
        // referenced on (sorted numerically). e.g. (aardvark: #1; FNORD: #3, #4; foo: #5; zygote: #1)
        val unresolvedStorages: Map[CasedSymbolName, Set[Storage]] = storageForwardReferenceFixups.unresolvedStorages()
        if (unresolvedStorages.nonEmpty) {
            val undefinedSymbolNamesSorted = unresolvedStorages.keySet.toList.sortWith((a: CasedSymbolName, b: CasedSymbolName) => {
                a.toString.compareToIgnoreCase(b.toString) < 0
            })
            val allStorageNamesAndLineReferences = undefinedSymbolNamesSorted.map((usn: CasedSymbolName) => {
                val storageSet = unresolvedStorages(usn)
                val storageLinesSorted = storageSet.map(_.indexedLine.location.lineNumber).toList.sorted
                val storageLineReferences = storageLinesSorted.map("#" + _).mkString(", ")
                val storageNameAndLineReferences = usn + ": " + storageLineReferences

                storageNameAndLineReferences
            })
            throw new AssemblyModelException("Storage forward references remain unresolved at end of Pass 1: (" +
              allStorageNamesAndLineReferences.mkString("; ") + ")")
        }


        val unresolvedSymbols = symbolForwardReferenceFixups.allUnresolvedSymbolForwardReferences()
        if (unresolvedSymbols.nonEmpty) {
            val undefinedSymbolNamesSorted = unresolvedSymbols.keySet.toList.sortWith((a: CasedSymbolName, b: CasedSymbolName) => {
                a.toString.compareToIgnoreCase(b.toString) < 0
            })
            val allStorageNamesAndLineReferences = undefinedSymbolNamesSorted.map((usn: CasedSymbolName) => {
                val unresolvableSymbols = unresolvedSymbols(usn).unresolvableSymbols
                val unresolvableSymbolLinesSorted = unresolvableSymbols.map(_.indexedLine.location.lineNumber).toList.sorted
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
            if (sourcedValuesForLineIndices.nonEmpty) {
                lowStorageAddress = Int.MaxValue
                highStorageAddress = Int.MinValue
                for (sourcedValues <- sourcedValuesForLineIndices.values) {
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
        // Uses lines storage
        foreachLineSourcedValues((indexedLine: IndexedLine, sourcedValues: List[SourcedValue]) => {
            logger.debug("----------")
            logger.debug(s"line $indexedLine")
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
