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

package org.devzendo.tma.output

import java.io.File

import org.devzendo.tma.ast.{DB, Line, Number}
import org.devzendo.tma.codegen.{AssemblyModel, Storage}
import org.devzendo.tma.util.TempFolder
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.mutable
import scala.io.Source

class TestListingWriter extends TempFolder with AssertionsForJUnit with MustMatchers {

    private val listingFile: File = File.createTempFile("out.", ".lst", temporaryDirectory)
    private val writer: ListingWriter = new ListingWriter(listingFile)
    private val model = new AssemblyModel()
    model.rows = 25
    model.columns = 80
    model.title = "Sample assembly listing"

    private def lineAccess(op: LineAccess => Unit) = {
        val la = new LineAccess()

        op(la)
    }

    private class LineAccess {
        private val lines: Array[String] = Source.fromFile(listingFile).getLines().toArray

        logger.debug(s"Read ${lines.length} line(s)")
        for (lineIndex <- lines.zipWithIndex) {
            val output = "%03d |%s|".format(lineIndex._2 + 1, lineIndex._1)
            logger.debug(output)
        }

        def line(number: Int): String = {
            if (lines.isEmpty) {
                throw new IllegalStateException("No lines were written to listing file")
            }
            lines(number)
        }
        def numLines = lines.length
    }

    private def topOfPageLinesList(numLinesTotal: Int): List[Int] = {
        val list = List.range(0, numLinesTotal, model.rows)
        logger.debug(s"top of page lines for $numLinesTotal total lines: $list")
        list
    }

    @Test
    def correctNumberOfTopOfPageLines(): Unit = {
        topOfPageLinesList(0) must be(List.empty)
        topOfPageLinesList(1) must be(List(0))
        topOfPageLinesList(model.rows) must be(List(0))
        topOfPageLinesList(model.rows + 1) must be(List(0, model.rows))
        topOfPageLinesList(model.rows * 2) must be(List(0, model.rows))
        topOfPageLinesList(model.rows * 2 + 1) must be(List(0, model.rows, model.rows * 2))
    }

    private def invariants(la: LineAccess): Unit = {
        for (l <- 0 until la.numLines) {
            val length = la.line(l).length
            length must be >= 0
            length must be <= model.columns
        }

        la.numLines must be >= 2 // at least one header
        la.numLines % model.rows must be(0) // padded out to page depth

        val topOfPageLines = topOfPageLinesList(la.numLines)
        for (topOfPageLine <- topOfPageLines) {
            logger.debug(s"checking top of page line $topOfPageLine")
            val firstLine = la.line(topOfPageLine)
            firstLine must have length model.columns
            firstLine must startWith regex """DevZendo\.org Macro Assembler \d+(\.\d+){2,3}(-SNAPSHOT)?"""
            firstLine must endWith regex """\d{2}/\d{2}/\d{4} \d{2}:\d{2}:\d{2}"""

            val secondLine = la.line(topOfPageLine + 1)
            secondLine must startWith (model.title)
            // page number (on the right) isn't invariant...
        }
    }

    @Test
    def emptyPage(): Unit = {
        writer.encode(model)
        // need at least one line to generate anything...
        lineAccess(la => {
            la.numLines must be(0)
        })
    }

    @Test
    def dontPadIfWrittenLinesCompletelyFillThePage(): Unit = {
        model.rows = 5
        for (index <- 1 to model.rows - 2) { // - 2 due to header/title
            val line = Line(index, "", None, None)
            model.addLine(line)
        }

        writer.encode(model)
        lineAccess(la => {
            invariants(la)

            la.numLines must be(5)
        })
    }

    @Test
    def singleLineOfStorageGivesSinglePage(): Unit = {
        // so there's at least one page; not bothered about storage / line content yet
        val exprs1 = List(Number(1), Number(2))
        val line = Line(2, "", None, Some(DB(exprs1)))
        model.addLine(line)
        model.allocateStorageForLine(line, 1, exprs1)

        writer.encode(model)
        lineAccess(la => {
            invariants(la)

            val secondLine = la.line(1)
            secondLine must endWith ("Page 1 - 1")
        })
    }

    @Test
    def differentPageNumbersOnSequentialPages(): Unit = {
        // three pages
        model.rows = 5
        for (index <- 1 to (15 - 6)) { // - 6 due to header/title
            val line = Line(index, "", None, None)
            model.addLine(line)
        }

        writer.encode(model)
        lineAccess(la => {
            invariants(la)

            val firstPage = la.line(1)
            firstPage must endWith ("Page 1 - 3")

            val secondPage = la.line(6)
            secondPage must endWith ("Page 2 - 3")

            val thirdPage = la.line(11)
            thirdPage must endWith ("Page 3 - 3")
        })
    }

    @Test
    def lineTruncation(): Unit = {
        import ListingWriter.truncateToMaxColumns
        // null conversion
        truncateToMaxColumns(null, 0) must be ("")
        truncateToMaxColumns(null, 1) must be ("")
        // emptiness
        truncateToMaxColumns("", 0) must be ("")
        // truncation at zero boundary
        truncateToMaxColumns("a", 0) must be ("")
        // no truncation necessary
        truncateToMaxColumns("a", 1) must be ("a")
        // truncation at non-zero boundary
        truncateToMaxColumns("ab", 1) must be ("a")
        // no truncation necessary, same width
        truncateToMaxColumns("ab", 2) must be ("ab")
        // no truncation necessary, plenty of space
        truncateToMaxColumns("ab", 3) must be ("ab")
    }

    @Test
    def lineTruncationWithLeftExpansionArea(): Unit = {
        model.columns = 70
        val line = Line(1, ";234567890123456789012345678901234567890123456789012345678901234567890", None, None)
        val leftExpansionArea = " " * 17
        model.addLine(line)

        writer.encode(model)
        lineAccess(la => {
            invariants(la)

            val thirdLine = la.line(2)
            thirdLine must be (leftExpansionArea + ";2345678901234567890123456789012345678901234567890123")
        })
    }

    @Test
    def printableLinesPerStorage(): Unit = {
        def widthAndIntsToStorage(cellWidth: Int, datas: Int): Storage = {
            val dataArray = Array.ofDim[Int](datas)
            Storage(0, cellWidth, dataArray, null, List.empty)
        }

        import ListingWriter.numPrintableLinesForStorage
        numPrintableLinesForStorage(widthAndIntsToStorage(1, 0)) must be(1)
        numPrintableLinesForStorage(widthAndIntsToStorage(1, 1)) must be(1)
        numPrintableLinesForStorage(widthAndIntsToStorage(1, 5)) must be(1)
        numPrintableLinesForStorage(widthAndIntsToStorage(1, 6)) must be(2)
        numPrintableLinesForStorage(widthAndIntsToStorage(1, 10)) must be(2)
        numPrintableLinesForStorage(widthAndIntsToStorage(1, 11)) must be(3)

        numPrintableLinesForStorage(widthAndIntsToStorage(2, 0)) must be(1)
        numPrintableLinesForStorage(widthAndIntsToStorage(2, 1)) must be(1)
        numPrintableLinesForStorage(widthAndIntsToStorage(2, 3)) must be(1)
        numPrintableLinesForStorage(widthAndIntsToStorage(2, 4)) must be(2)
        numPrintableLinesForStorage(widthAndIntsToStorage(2, 6)) must be(2)
        numPrintableLinesForStorage(widthAndIntsToStorage(2, 7)) must be(3)

        numPrintableLinesForStorage(widthAndIntsToStorage(4, 0)) must be(1)
        numPrintableLinesForStorage(widthAndIntsToStorage(4, 1)) must be(1)
        numPrintableLinesForStorage(widthAndIntsToStorage(4, 2)) must be(2)
        numPrintableLinesForStorage(widthAndIntsToStorage(4, 3)) must be(3)
    }
}
