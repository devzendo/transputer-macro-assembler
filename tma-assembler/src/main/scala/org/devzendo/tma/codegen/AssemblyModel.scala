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

import org.devzendo.tma.ast.{Expression, Number, SymbolArg}

import scala.collection.mutable

/*
 * A mutable structure holding the output of the CodeGenerator.
 */
class AssemblyModel {

    private val dollar = "$"

    var title = ""
    var rows = 25
    var columns = 80
    var processor: Option[String] = None

    private val variables = mutable.HashMap[String, Int]()

    variables.put(dollar, 0)

    def getDollar: Int = getVariable(dollar)
    def setDollar(n: Int): Unit = {
        setVariable(dollar, n)
    }

    def getVariable(name: String): Int = {
        variables.get(name) match {
            case Some(i) => i
            case None => throw new AssemblyModelException("Variable '" + name + "' has not been defined")
        }
    }
    def variable(name: String): Option[Int] = variables.get(name)
    def setVariable(name: String, n: Int): Unit = {
        variables.put(name, n)
    }

    /**
      * Evaluate an expression, returning Left(List(undefined variable names)) or Right(value)
      * @param expr some expression, of any complexity
      * @return undefined variable names, or the evaluated value.
      */
    def evaluateExpression(expr: Expression): Either[List[String], Int] = {
        expr match {
            case Number(n) => Right(n)
            case SymbolArg(name) =>
                variable(name) match {
                    case Some(n) => Right(n)
                    case None => Left(List(name))
                }
        }
    }

}
