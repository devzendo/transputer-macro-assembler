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

package org.devzendo.tma

import org.devzendo.tma.ast.Line
import org.devzendo.tma.codegen.{AssemblyModel, CodeGenerator, OffsetTransformer}
import org.devzendo.tma.parser.{AssemblyParser, MacroManager}

import scala.collection.mutable

trait AssemblerFixture {

    // Higher-level, not from Lines, but from text input to parse into Lines... do a 'proper' assembly...
    def assemble(linesToParse: List[String]): AssemblyModel = {
        val macroManager = new MacroManager(true)
        val parser = new AssemblyParser(true, true, macroManager)
        val model = new AssemblyModel(true)
        val codegen = new CodeGenerator(true, model)
        codegen.addStatementTransformer(new OffsetTransformer(model).transform)

        val parsedLinesSoFar = mutable.ArrayBuffer[Line]()
        linesToParse.zipWithIndex.foreach((p: (String, Int)) => {
            parsedLinesSoFar ++= parser.parse(p._1, p._2 + 1)
        })
        codegen.createModel(parsedLinesSoFar.toList)
        codegen.endCheck()

        model
    }
}
