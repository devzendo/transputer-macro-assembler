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

package org.devzendo.tma

import java.io.File

import org.devzendo.tma.ast.Line
import org.devzendo.tma.codegen.{AssemblyModel, CodeGenerationException, CodeGenerator}
import org.devzendo.tma.output.{BinaryWriter, ELFWriter, ListingWriter}
import org.devzendo.tma.parser.{AssemblyParser, AssemblyParserException}
import org.log4s.Logger

import scala.collection.mutable
import scala.io.Source

class AssemblerController(parser: AssemblyParser, codegen: CodeGenerator) {
    private val logger: Logger = org.log4s.getLogger

    // PARSING PHASE ---------------------------------------------------------------------------------------------------

    private val parseErrors = mutable.ArrayBuffer[AssemblyParserException]()
    private val parsedLinesSoFar = mutable.ArrayBuffer[Line]()

    // Input is either read from a text file, and parsed, to form the parsedLines list....
    def parseFile(inputFile: File): Unit = {
        // line numbers for humans start at 1, hence p._2 + 1
        val includer = new SourceIncludingReader
        val sourceItems = includer.openSourceIterator(inputFile)
        sourceItems.foreach( (si: SourceItem) => parseTextLine(si.lineNumber, si.line))
    }

    def parseTextLine(lineNumber: Int, text: String): Unit = {
        try {
            val lineList = parser.parse(text, lineNumber)
            parsedLinesSoFar ++= lineList
        } catch {
            case ape: AssemblyParserException =>
                parseErrors += ape
        }
    }

    // ... or it is pre-parsed (perhaps created by a higher-level language), and just added.
    def addParsedLine(line: Line): Unit = {
        parsedLinesSoFar += line
    }

    // Obtain any parsing errors that have been accumulated. If there are errors, the main code will log them and exit.
    def getParseExceptions: List[AssemblyParserException] = parseErrors.toList

    // CODE GENERATION PHASE -------------------------------------------------------------------------------------------

    def generateModel(): AssemblyModel = {
        if (parsedLinesSoFar.isEmpty) {
            throw new RuntimeException("No parsed input")
        }
        if (parseErrors.nonEmpty) {
            throw new RuntimeException("Parse errors; cannot continue")
        }
        val model = codegen.createModel(parsedLinesSoFar.toList)
        codegen.endCheck()
        model
    }

    // Obtain any code generation errors that have been accumulated. If there are errors, the main code will log them
    // and exit.
    def getCodeGenerationExceptions: List[CodeGenerationException] = codegen.getCodeGenerationExceptions

    // OUTPUT PHASE ----------------------------------------------------------------------------------------------------

    def output(model: AssemblyModel, outputFile: Option[File], binaryFile: Option[File], listingFile: Option[File]): Unit = {
        // TODO throw if model not generated...
        outputFile.foreach(new ELFWriter(_).encode(model))
        binaryFile.foreach(new BinaryWriter(_).encode(model))
        listingFile.foreach(new ListingWriter(_).encode(model))
    }
}
