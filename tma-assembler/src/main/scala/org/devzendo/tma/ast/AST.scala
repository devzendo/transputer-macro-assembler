/*
 * Copyright (C) 2008-2017 Matt Gumbley, DevZendo.org http://devzendo.org
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

package org.devzendo.tma.ast

import org.devzendo.tma.ast.AST._

object AST {
    type Opcode = Int
    type Label = String
    type SymbolName = String
    type MacroName = String
    type MacroArgName = String
}

sealed abstract class Operator(op: String) {
    override def toString: Label = op
}
case class Add() extends Operator("+")
case class Sub() extends Operator("-")
case class Negate() extends Operator("-")
case class Mult() extends Operator("*")
case class Div() extends Operator("/")
case class ShiftLeft() extends Operator("<<")
case class ShiftRight() extends Operator(">>")
case class And() extends Operator("&")
case class Or() extends Operator("|")
case class Xor() extends Operator("^")
case class Not() extends Operator("~")


sealed abstract class Expression() {
}
case class SymbolArg(symbolName: SymbolName) extends Expression
case class Number(number: Int) extends Expression
case class Unary(operator: Operator, expr: Expression) extends Expression
case class Binary(operator: Operator, left: Expression, right: Expression) extends Expression


sealed abstract class Statement() {
}
case class Title(title: String) extends Statement
case class Page(rows: Int, columns: Int) extends Statement
case class Processor(processor: String) extends Statement
case class Align(num: Int) extends Statement
case class Org(expr: Expression) extends Statement
case class ConstantAssignment(symbolName: SymbolName, expr: Expression) extends Statement
case class VariableAssignment(symbolName: SymbolName, expr: Expression) extends Statement
case class MacroStart(name: MacroName, argNames: List[MacroArgName]) extends Statement
case class MacroBody(line: String) extends Statement
case class MacroEnd() extends Statement
case class MacroInvocation(name: MacroName, args: List[Expression]) extends Statement
case class DB(expr: Expression *) extends Statement
case class DW(expr: Expression *) extends Statement
case class DD(expr: Expression *) extends Statement
case class DirectInstruction(opcode: Opcode, num: Int) extends Statement
case class IndirectInstruction(opcode: Opcode) extends Statement


case class Memory(address: Int, data: List[Byte])


case class Line(number: Int, text: String, label: Option[Label], stmt: Option[Statement]) {
}

case class MacroDefinition(name: MacroName, argNames: List[MacroArgName], textLines: List[String])