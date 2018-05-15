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
import org.log4s.Logger

class CodeGenerator(debugCodegen: Boolean) {

    val logger: Logger = org.log4s.getLogger

    private val model = new AssemblyModel

    def processLine(line: Line): Unit = {
        line.stmt.foreach ( (stmt: Statement) =>
            processStatement(stmt)
        )
    }


    private def processStatement(stmt: Statement) = {
        stmt match {
            case Title(text) => model.title = text
            case Page(rows, columns) => {
                model.rows = rows
                model.columns = columns
            }
            case Processor(name) => model.processor = Some(name)
            case Org(expr) => processOrg(expr)

            case Ignored() => // Do nothing
        }
    }

    private def processOrg(expr: Expression) = {
        val either = model.evaluateExpression(expr)
        either match {
            case Right(n) => model.setDollar(n)
        }
    }

    def createModel(lines: List[Line]): AssemblyModel = {
        logger.debug("Creating model from " + lines.size + " line(s)")

        lines.foreach( (l: Line) => processLine(l) )
        model
    }
}
