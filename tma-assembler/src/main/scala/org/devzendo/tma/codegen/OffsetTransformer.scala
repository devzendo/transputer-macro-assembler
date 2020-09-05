/*
 * Copyright (C) 2008-2020 Matt Gumbley, DevZendo.org http://devzendo.org
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

import org.devzendo.tma.ast._
import org.devzendo.tma.TransputerDirectInstructions
import org.log4s.Logger

/**
  * Transforms Statements by converting embedded Offsets in expressions (which have no address state) to OffsetFroms
  * (which are relative to an address). Also converts DxDUP to a list of repeated expressions that can then be
  * encoded as Dx - whilst evaluating the address of any Offset/OffsetFrom in them.
  *
  * Other Statements are not transformed.
  *
  * @param model the current AssemblyModel from which $ will be retrieved.
  */
class OffsetTransformer(model: AssemblyModel) extends TransputerDirectInstructions {
    val logger: Logger = org.log4s.getLogger

    def transform(stmt: Statement): Statement = {
        stmt match {
            case Org(expr) => Org(convertOffsets(expr))
            case ConstantAssignment(name, expr) => ConstantAssignment(name, convertOffsets(expr))
            case VariableAssignment(name, expr) => VariableAssignment(name, convertOffsets(expr))
            case DB(exprs) => DB(convertListOfOffsets(exprs, 1))
            case DW(exprs) => DW(convertListOfOffsets(exprs, 2))
            case DD(exprs) => DD(convertListOfOffsets(exprs, 4))
            case DBDup(count, repeatedExpr) => DB(convertRepeatedOffsets(count, repeatedExpr, 1))
            case DWDup(count, repeatedExpr) => DW(convertRepeatedOffsets(count, repeatedExpr, 2))
            case DDDup(count, repeatedExpr) => DD(convertRepeatedOffsets(count, repeatedExpr, 4))
            case DirectInstruction(opcode, opbyte, expr) => {
                opbyte match {
                    case OP_J | OP_CJ | OP_CALL =>
                        DirectInstruction(opcode, opbyte, convertSymbolArgToOffset(expr))
                    case _ =>
                        DirectInstruction(opcode, opbyte, convertOffsets(expr))
                }
            }

            case _ => stmt
        }
    }

    // If an expression contains just a symbol, replace it with an offset to the symbol, but note that in a direct
    // instruction, IPtr will have been incremented after the decode (which may have been preceeded by PFIX/NFIXes,
    // so the dollar used in the offset is the start of the instruction.
    // $ is the start of the variable length - potentially 8 byte - instruction. However when the direct instruction
    // is executed, IPtr will be the address after the 'real' opcode (J, CJ, LDC etc.).
    // 1000: ldc 0x1234abcd => 0x21, 0x22, 0x23, 0x24, 0x2a, 0x2b, 0x2c, 0x4d
    // $ would be 1000 but after execution when an offset is needed, IPtr now being 1007.
    def convertSymbolArgToOffset(expr: Expression, dollar: Int = model.getDollar): Expression = {
        logger.debug("convertSymbolArgToOffset(" + expr + ") $=" + dollar)
        expr match {
            case SymbolArg(name) => {
                Unary(OffsetFrom(dollar), expr)
            }
            case Unary(op, uExpr) =>
                op match {
                    case Offset() => Unary(OffsetFrom(dollar), uExpr)
                    case _ => expr
                }
            case _ => expr
        }
    }

    // If an expression contains an Offset, convert it to an OffsetFrom with a given (defaulted) $.
    def convertOffsets(expr: Expression, dollar: Int = model.getDollar): Expression = {
        expr match {
            case Unary(op, uExpr) =>
                op match {
                    case Offset() => Unary(OffsetFrom(dollar), uExpr)
                    case _ => expr
                }
            case _ => expr
        }
    }

    // Convert a single expression that's to be repeated several times into a list of expressions that have an increased $ mapped across them.
    def convertRepeatedOffsets(count: Expression, repeatedExpr: Expression, cellWidth: Int): List[Expression] = {
        model.evaluateExpression(convertOffsets(count)) match {
            case Left(_) =>
                throw new StatementTransformationException("Count of '" + count + "' is undefined")
            case Right(evaluatedCount) =>
                val copiesOfRepeatedExpr = List.fill(evaluatedCount)(repeatedExpr)
                convertListOfOffsets(copiesOfRepeatedExpr, cellWidth)
        }
    }

    // Apply an increasing $ across a list of expressions.
    def convertListOfOffsets(exprs: List[Expression], cellWidth: Int): List[Expression] = {
        exprs.zipWithIndex.map((pair: (Expression, Int)) => convertOffsets(pair._1, model.getDollar + (cellWidth * pair._2)) )
    }
}
