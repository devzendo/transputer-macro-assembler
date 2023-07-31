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

import org.devzendo.tma.SourceLocation
import org.devzendo.tma.ast.{IndexedLine, Line, Statement}

abstract class CodeGeneratorFixture {

    val model = new AssemblyModel(true)
    val codegen = new CodeGenerator(true, model)

    def generateFromStatements(stmts: List[Statement]): AssemblyModel = {
        val stmts2Lines = stmts.zipWithIndex.map((p: (Statement, Int)) => Line(SourceLocation("", p._2 + 1), p._1.toString, None, Some(p._1)))
        generateFromLines(stmts2Lines)
    }

    def generateFromStatement(stmt: Statement): AssemblyModel = {
        generateFromLine(Line(SourceLocation("", 1), "", None, Some(stmt)))
    }

    def generateFromLine(line: Line): AssemblyModel = {
        generateFromLines(List(line))
    }

    def generateFromLines(lines: List[Line]): AssemblyModel = {
        val model = codegen.createModel(lines)
        // In this test class, we want to catch/sense the first exception. When used in the main assembler code,
        // all exceptions are caught and logged so we can see the full error list, not just the first.
        val exceptions = codegen.getCodeGenerationExceptions
        if (exceptions.nonEmpty) {
            throw exceptions.head
        } else {
            model
        }
    }
}
