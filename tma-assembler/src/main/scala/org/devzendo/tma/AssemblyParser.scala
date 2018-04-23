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

package org.devzendo.tma

import org.devzendo.tma.ast.Line

import scala.collection.mutable
import scala.util.parsing.combinator._

class AssemblyParser(val debugParser: Boolean) {

    val logger = org.log4s.getLogger

    val lines = mutable.ArrayBuffer[Line]()

    def getLines() = {
        lines.toList
    }

    @throws(classOf[AssemblyParserException])
    def parse(lineAndNumber: (String, Int)) = {
        val line = lineAndNumber._1
        val number = lineAndNumber._2
        def sanitizedInput = nullToEmpty(line).trim()
        if (debugParser) {
            logger.debug("parsing " + number + "|" + sanitizedInput + "|")
        }
        if (number < 1) {
            throw new AssemblyParserException("Line numbers must be positive")
        }
//        if (sanitizedInput.size > 0) {
            lines += Line(number, sanitizedInput, List.empty)
//        }
    }

    private def nullToEmpty(input: String): String = {
        if (input == null) "" else input

    }

    def createModel: AssemblyModel = {
        new AssemblyModel
    }
}
