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

import org.devzendo.tma.ast._

/**
  * Transforms Statements by converting embedded Offsets in expressions (which have no address state) to OffsetFroms
  * (which are relative to an address). Also converts DxDUP to a list of repeated expressions that can then be
  * encoded as Dx - whilst evaluating the address of any Offset/OffsetFrom in them.
  *
  * Other Statements are not transformed.
  *
  * @param model the current AssemblyModel from which $ will be retrieved.
  */
class OffsetTransformer(model: AssemblyModel) {

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
            case DirectInstruction(opcode, opbyte, expr) => DirectInstruction(opcode, opbyte, convertOffsets(expr))

            case _ => stmt
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
