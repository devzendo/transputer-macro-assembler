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

import org.devzendo.tma.ast.AST.SymbolName
import org.devzendo.tma.ast._
import org.devzendo.tma.codegen.{AssemblyModel, AssignmentValue, Storage}
import org.devzendo.tma.util.TempFolder
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

import scala.io.Source

class TestListingWriter extends TempFolder with AssertionsForJUnit with MustMatchers {

    private val listingFile: File = File.createTempFile("out.", ".lst", temporaryDirectory)
    private val writer: ListingWriter = new ListingWriter(listingFile)
    private val model = new AssemblyModel()
    private val fnord = "FNORD"

    model.rows = 25
    model.columns = 80
    model.title = "Sample assembly listing"

    private def lineAccess(op: LineAccess => Unit): Unit = {
        val la = new LineAccess()

        op(la)
    }

    private class LineAccess {
        private val lines: Array[String] = Source.fromFile(listingFile).getLines().toArray

        logger.debug(s"Read ${lines.length} line(s)")

        // print columns
        private val colRowsHeight = ("" + model.columns).length
        logger.debug(s"Columns ${model.columns} x Rows: ${model.rows}; column rows height: $colRowsHeight")
        private val columnRows = Array.ofDim[Char](colRowsHeight, model.columns)
        private val fmt = s"%0${colRowsHeight}d"
        for (x <- 0 until model.columns) {
            val vStr = fmt.format(x + 1)
            for (y <- 0 until colRowsHeight) {
                val c = vStr(y)
                columnRows(y)(x) = c
            }
        }
        for (y <- 0 until colRowsHeight) {
            val line = columnRows(y).mkString
            logger.debug(s"   |$line|")
        }

        for (lineIndex <- lines.zipWithIndex) {
            val output = "%03d|%s|".format(lineIndex._2 + 1, lineIndex._1)
            logger.debug(output)
        }

        def line(number: Int): String = {
            if (lines.isEmpty) {
                throw new IllegalStateException("No lines were written to listing file")
            }
            lines(number)
        }
        def numLines: Int = lines.length
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

    private def listingBodyLinesAre(expectedLines: String*): Unit = {
        model.rows = 2 + expectedLines.length
        writer.encode(model)
        lineAccess(la => {
            invariants(la)

            for (line <- 2 until 2 + expectedLines.length) { // first page
                val contentLine = la.line(line)
                contentLine must be(expectedLines(line - 2))
            }
        })
    }

    private def symbolTableBodyLinesAre(expectedLines: String*): Unit = {
        model.rows = 2 + expectedLines.length
        writer.encode(model)
        lineAccess(la => {
            invariants(la)

            for (line <- model.rows + 2 until model.rows + 2 + expectedLines.length) { // second page
                //logger.debug(s"symbol table line $line")
                val contentLine = la.line(line)
                //logger.debug(s"actual '$contentLine'")
                val expectedLine = expectedLines(line - 2 - model.rows)
                //logger.debug(s"expected '$expectedLine'")
                contentLine must be(expectedLine)
            }
        })
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
    def lengthPadding(): Unit = {
        import ListingWriter.padToLength
        padToLength(null, 0) must be("")
        padToLength("", 0) must be("")

        padToLength("x", 0) must be("")

        padToLength(null, 1) must be(" ")
        padToLength("", 1) must be(" ")

        padToLength("x", 1) must be("x")
        padToLength("x", 2) must be("x ")
        padToLength("x", 3) must be("x  ")

        padToLength("xy", 2) must be("xy")

        padToLength("xyz", 2) must be("xy")
    }

    @Test
    def lineTruncationWithLeftExpansionArea(): Unit = {
        model.columns = 70
        val line = Line(1, ";234567890123456789012345678901234567890123456789012345678901234567890", None, None)
        val leftExpansionArea = " " * 22
        model.addLine(line)

        writer.encode(model)
        lineAccess(la => {
            invariants(la)

            val thirdLine = la.line(2)
            thirdLine must be (leftExpansionArea + ";23456789012345678901234567890123456789012345678")
        })
    }

    @Test
    def printableLinesPerStorage(): Unit = {
        def widthAndIntsToStorage(cellWidth: Int, datas: Int): Storage = {
            val dataArray = Array.ofDim[Int](datas)
            Storage(0, cellWidth, dataArray, null, List.empty)
        }

        import ListingWriter.numPrintableLinesForSourcedValue
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(1, 0)) must be(1)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(1, 1)) must be(1)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(1, 4)) must be(1)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(1, 5)) must be(2)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(1, 8)) must be(2)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(1, 9)) must be(3)

        numPrintableLinesForSourcedValue(widthAndIntsToStorage(2, 0)) must be(1)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(2, 1)) must be(1)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(2, 3)) must be(1)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(2, 4)) must be(2)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(2, 6)) must be(2)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(2, 7)) must be(3)

        numPrintableLinesForSourcedValue(widthAndIntsToStorage(4, 0)) must be(1)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(4, 1)) must be(1)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(4, 2)) must be(2)
        numPrintableLinesForSourcedValue(widthAndIntsToStorage(4, 3)) must be(3)

        numPrintableLinesForSourcedValue(AssignmentValue(0, null, isLabel = false)) must be(1)
    }

    @Test
    def labelsShowTheirAddresses(): Unit = {
        model.setDollarSilently(0x40000000)
        val line = Line(1, "FNORD:", Some(fnord), None)
        model.addLine(line)
        model.setLabel(fnord, model.getDollar, line)
        //                  123456789012345678901234567890
        val expectedLine = " 40000000             FNORD:"
        listingBodyLinesAre(expectedLine)
    }

    @Test
    def assignmentOf16BitNumber(): Unit = {
        val line = Line(1, "FNORD = 65534", None, Some(VariableAssignment(new SymbolName(fnord), Number(65534))))
        model.addLine(line)
        model.setVariable(fnord, 65534, line)
        //                  12345678901234567890123456789012345
        val expectedLine = " = FFFE               FNORD = 65534"
        listingBodyLinesAre(expectedLine)
    }

    @Test
    def assignmentOf32BitNumber(): Unit = {
        val line = Line(1, "FNORD = 65536", None, Some(VariableAssignment(new SymbolName(fnord), Number(65536))))
        model.addLine(line)
        model.setVariable(fnord, 65536, line)
        //                  12345678901234567890123456789012345
        val expectedLine = " = 00010000           FNORD = 65536"
        listingBodyLinesAre(expectedLine)
    }

    @Test
    def storageShowsItsAddress(): Unit = {
        model.setDollarSilently(0x40000000)
        val exprs = List(Number(1), Number(2), Number(3))
        val line = Line(1, "DB 1,2,3", None, Some(DB(exprs)))
        model.addLine(line)
        model.allocateStorageForLine(line, 1, exprs)
        //                  12345678901234567890123456789012345
        val expectedLine = " 40000000 01 02 03    DB 1,2,3"
        listingBodyLinesAre(expectedLine)
    }

    @Test
    def storageMaxBytesPerLine(): Unit = {
        val exprs = List(Number(1), Number(2), Number(3), Number(4))
        val line = Line(1, "DB 1,2,3,4", None, Some(DB(exprs)))
        model.addLine(line)
        model.allocateStorageForLine(line, 1, exprs)
        //                  12345678901234567890123456789012345
        val expectedLine = " 00000000 01 02 03 04 DB 1,2,3,4"
        listingBodyLinesAre(expectedLine)
    }

    @Test
    def storageMaxWordsPerLine(): Unit = {
        val exprs = List(Number(0x0102), Number(0x0304))
        val line = Line(1, "DW 0x0102,0x0304", None, Some(DW(exprs)))
        model.addLine(line)
        model.allocateStorageForLine(line, 2, exprs)
        //                  12345678901234567890123456789012345
        val expectedLine = " 00000000 0102 0304   DW 0x0102,0x0304"
        listingBodyLinesAre(expectedLine)
    }

    @Test
    def storageMaxDoubleWordsPerLine(): Unit = {
        val exprs = List(Number(0x01020304))
        val line = Line(1, "DD 0x01020304", None, Some(DD(exprs)))
        model.addLine(line)
        model.allocateStorageForLine(line, 4, exprs)
        //                  12345678901234567890123456789012345
        val expectedLine = " 00000000 01020304    DD 0x01020304"
        listingBodyLinesAre(expectedLine)
    }

    @Test
    def storageOverMaxBytesPerLine(): Unit = {
        val exprs = List(Number(1), Number(2), Number(3), Number(4), Number(5), Number(6), Number(7), Number(8), Number(9))
        val line = Line(1, "DB 1,2,3,4,5,6,7,8,9", None, Some(DB(exprs)))
        model.addLine(line)
        model.allocateStorageForLine(line, 1, exprs)
        //                   12345678901234567890123456789012345
        val expectedLine1 = " 00000000 01 02 03 04 DB 1,2,3,4,5,6,7,8,9"
        val expectedLine2 = " 00000004 05 06 07 08"
        val expectedLine3 = " 00000008 09"
        listingBodyLinesAre(expectedLine1, expectedLine2, expectedLine3)
    }

    @Test
    def storageOverMaxWordsPerLine(): Unit = {
        val exprs = List(Number(0x0102), Number(0x0304), Number(0x0506), Number(0x0708), Number(0x090A))
        val line = Line(1, "DW 0x0102,0x0304,0x0506,0x0708,0x090A", None, Some(DW(exprs)))
        model.addLine(line)
        model.allocateStorageForLine(line, 2, exprs)
        //                   12345678901234567890123456789012345
        val expectedLine1 = " 00000000 0102 0304   DW 0x0102,0x0304,0x0506,0x0708,0x090A"
        val expectedLine2 = " 00000004 0506 0708"
        val expectedLine3 = " 00000008 090A"
        listingBodyLinesAre(expectedLine1, expectedLine2, expectedLine3)
    }

    @Test
    def storageOverMaxDoubleWordsPerLine(): Unit = {
        val exprs = List(Number(0x01020304), Number(0x05060708), Number(0x090A0B0C))
        val line = Line(1, "DD 0x01020304,0x05060708,0x090A0B0C", None, Some(DD(exprs)))
        model.addLine(line)
        model.allocateStorageForLine(line, 4, exprs)
        //                   12345678901234567890123456789012345
        val expectedLine1 = " 00000000 01020304    DD 0x01020304,0x05060708,0x090A0B0C"
        val expectedLine2 = " 00000004 05060708"
        val expectedLine3 = " 00000008 090A0B0C"
        listingBodyLinesAre(expectedLine1, expectedLine2, expectedLine3)
    }

    @Test
    def labelsShownInSymbolTable(): Unit = {
        model.setDollarSilently(0x40000000)
        val line1 = Line(1, "FNORD:", Some(fnord), None)
        model.addLine(line1)
        model.setLabel(fnord, model.getDollar, line1)
        model.setDollarSilently(0x40000020)
        val line2 = Line(2, "AARDVARK:", Some("AARDVARK"), None)
        model.addLine(line2)
        model.setLabel("AARDVARK", model.getDollar, line2)

        checkSymbolTableContents()
    }

    @Test
    def constantsShownInSymbolTable(): Unit = {
        val line1 = Line(1, "EQU FNORD 0x40000020", None, Some(ConstantAssignment(new SymbolName(fnord), Number(0x40000020))))
        model.addLine(line1)
        model.setConstant(fnord, 0x40000000, line1)
        val line2 = Line(2, "EQL AARDVARK 0x40000000", None, Some(ConstantAssignment(new SymbolName("AARDVARK"), Number(0x40000010))))
        model.addLine(line2)
        model.setConstant("AARDVARK", 0x40000020, line2)

        checkSymbolTableContents()
    }

    private def checkSymbolTableContents(): Unit = {
        //                  123456789012345678901234567890
        val expectedLine1 = "Symbol Table - by Name"
        val expectedLine2 = "AARDVARK             40000020"
        val expectedLine3 = "FNORD                40000000"
        val expectedLine4 = ""
        val expectedLine5 = "Symbol Table - by Address"
        val expectedLine6 = "FNORD                40000000"
        val expectedLine7 = "AARDVARK             40000020"
        symbolTableBodyLinesAre(expectedLine1, expectedLine2, expectedLine3, expectedLine4, expectedLine5,
            expectedLine6, expectedLine7)
    }
}
