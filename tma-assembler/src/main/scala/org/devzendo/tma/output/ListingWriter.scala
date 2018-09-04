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

import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.devzendo.commoncode.string.HexDump
import org.devzendo.tma.Version
import org.devzendo.tma.ast.Line
import org.devzendo.tma.codegen.{AssemblyModel, AssignmentValue, SourcedValue, Storage, SymbolType}

object ListingWriter {
    def numPrintableLinesForSourcedValue(sourcedValue: SourcedValue): Int = {
        sourcedValue match {
            case storage: Storage =>
                if (storage.data.length == 0) {
                    1
                } else {
                    storage.cellWidth match {
                        case 1 => (storage.data.length + 3) / 4
                        case 2 => (storage.data.length + 2) / 3
                        case 4 => storage.data.length
                    }
                }
            case _: AssignmentValue =>
                1
        }
    }

    def truncateToMaxColumns(line: String, maxColumns: Int): String = {
        val inLine = if (line == null) "" else line

        if (inLine.length > maxColumns) {
            inLine.substring(0, maxColumns)
        } else {
            inLine
        }
    }

    def padToLength(line: String, length: Int): String = {
        val tr = truncateToMaxColumns(line, length)
        if (tr.length < length) {
            tr + (" " * (length - tr.length))
        } else {
            tr
        }
    }
}

class ListingWriter(val outputFile: File) {
    import ListingWriter._

    private val logger = org.log4s.getLogger
    private val headerLeft = s"DevZendo.org Macro Assembler ${Version.getPropertiesVersion()}"
    private val headerRight = LocalDateTime.now().format(DateTimeFormatter.ofPattern("kk/MM/yyyy HH:mm:ss"))

    def encode(model: AssemblyModel): Unit = {

        def calculatePrintableListingLines(): Int = {
            var printableLines = 0
            model.foreachLineSourcedValues((_: Line, sourcedValues: List[SourcedValue]) => {
                if (sourcedValues.nonEmpty) {
                    for (sourcedValue <- sourcedValues) {
                        printableLines += numPrintableLinesForSourcedValue(sourcedValue)
                    }
                } else {
                    // just count the line, there's no subsequent byte lines
                    printableLines += 1
                }
            })
            printableLines
        }

        def calculateSymbolTableLines(): Int = {
            var printableLines = 0
            val symbols = model.getSymbols
            if (symbols.nonEmpty) {
                printableLines += 3 // 2 headers and a gap
                printableLines += symbols.size * 2 // sorted by address and name
            }
            printableLines
        }

        logger.info("Writing listing file " + outputFile.getName)
        val header = headerLeft + (" " * (model.columns - headerLeft.length - headerRight.length)) + headerRight

        var lineNumber = 0
        var pageNumber = 1

        val pageHeightWithoutHeader = model.rows - 2

        val printableListingLines = calculatePrintableListingLines()
        val printableListingPages = (printableListingLines + pageHeightWithoutHeader - 1) / pageHeightWithoutHeader

        val printableSymbolTableLines = calculateSymbolTableLines()
        val printableSymbolTablePages = (printableSymbolTableLines + pageHeightWithoutHeader - 1) / pageHeightWithoutHeader

        val maxPageNumber = printableListingPages + printableSymbolTablePages
        logger.debug(s"printable listing lines $printableListingLines printable symbol table lines $printableSymbolTableLines")
        logger.debug(s"printable listing pages $printableListingPages printable symbol table pages $printableSymbolTablePages")
        logger.debug("maxPageNumber $maxPageNumber pageHeightWithoutHeader $pageHeightWithoutHeader")

        val printWriter = new PrintWriter(outputFile)
        try {
            // First, the Listing...
            model.foreachLineSourcedValues((line: Line, sourcedValues: List[SourcedValue]) => {

                val lineBuf = new StringBuilder()

                var initialStorageAddress = 0
                var address = ""
                var assignment = ""
                var storageLines: List[String] = List.empty
                for (sourcedValue <- sourcedValues) {
                    logger.debug(s"sourcedValue $sourcedValue")
                    sourcedValue match {
                        case storage: Storage =>
                            initialStorageAddress = storage.address
                            address = HexDump.int2hex(storage.address)
                            storageLines = storage.cellWidth match {
                                case 1 =>
                                    val eachLinesInts = storage.data.sliding(4, 4)
                                    eachLinesInts.map((a: Array[Int]) => a.map((b: Int) => HexDump.byte2hex(b.toByte)).mkString(" ")).toList
                                case 2 =>
                                    val eachLinesInts = storage.data.sliding(2, 2)
                                    eachLinesInts.map((a: Array[Int]) => a.map((b: Int) => HexDump.short2hex(b.toShort)).mkString(" ")).toList
                                case 4 =>
                                    val eachLinesInts = storage.data.sliding(1, 1)
                                    eachLinesInts.map((a: Array[Int]) => a.map((b: Int) => HexDump.int2hex(b)).mkString(" ")).toList
                            }
                        case assignmentValue: AssignmentValue =>
                            if (assignmentValue.symbolType == SymbolType.Label) {
                                address = HexDump.int2hex(assignmentValue.data)
                            } else {
                                assignment = "= " +
                                  (if (assignmentValue.data <= 65535)
                                      HexDump.short2hex(assignmentValue.data.toShort)
                                  else
                                      HexDump.int2hex(assignmentValue.data))
                            }
                    }
                }

                val firstStorageLine = if (storageLines.isEmpty) "" else storageLines.head
                val left = padToLength(" " + List(address, assignment, firstStorageLine).filter(_.nonEmpty).mkString(" "), 22)
                logger.debug(s"address '$address' assignment '$assignment' left '$left'")
                lineBuf.append(left)
                lineBuf.append(line.text)
                emitLineWithHeader(lineBuf.toString())

                if (storageLines.size > 1) {
                    for (remainingStorageLine <- storageLines.tail) {
                        initialStorageAddress += 4
                        lineBuf.clear()
                        lineBuf.append(" ")
                        lineBuf.append(HexDump.int2hex(initialStorageAddress))
                        lineBuf.append(" ")
                        lineBuf.append(remainingStorageLine)
                        emitLineWithHeader(lineBuf.toString())
                    }
                }
            })

            padOutToFullPages()

            // Secondly, the symbol table, sorted by name, then address.
            val labels = model.getSymbols
            if (labels.nonEmpty) {
                val lineBuf = new StringBuilder()

                emitLineWithHeader("Symbol Table - by Name")
                val labelsByName = labels.sortWith(_.name < _.name)
                for (l <- labelsByName) {
                    lineBuf.clear()
                    lineBuf.append(padToLength(l.name, 21))
                    lineBuf.append(HexDump.int2hex(l.value))
                    emitLineWithHeader(lineBuf.toString())
                }

                emitLineWithHeader("")
                emitLineWithHeader("Symbol Table - by Address")
                val labelsByAddress = labels.sortWith(_.value < _.value)
                for (l <- labelsByAddress) {
                    lineBuf.clear()
                    lineBuf.append(padToLength(l.name, 21))
                    lineBuf.append(HexDump.int2hex(l.value))
                    emitLineWithHeader(lineBuf.toString())
                }

                padOutToFullPages()
            }

        } finally {
            printWriter.close()
        }


        def padOutToFullPages(): Unit = {
            // Only pad out if anything at all has been written...
            val remainder = lineNumber % model.rows
            if (lineNumber > 0 && remainder != 0) {
                val remainingPageRows = model.rows - remainder
                logger.debug(s"padding: end line number $lineNumber model.rows ${model.rows} remaining page rows $remainingPageRows")
                for (_ <- 0 until remainingPageRows) {
                    emitLineWithHeader("")
                }
            }
        }

        def emitLineWithHeader(line: String): Unit = {
            if (lineNumber % model.rows == 0) {
                logger.debug(s"#$lineNumber: header")
                printLine(header)

                val pageNumbers = s"Page $pageNumber - $maxPageNumber"
                logger.debug(s"#$lineNumber: title/pages $pageNumbers")
                val titlePageNumbers = model.title + (" " * (model.columns - model.title.length - pageNumbers.length)) + pageNumbers
                printLine(titlePageNumbers)
                pageNumber += 1
            }

            logger.debug(s"#$lineNumber: |$line|")

            printLine(line)
        }

        def printLine(line: String): Unit = {
            printWriter.println(truncateToMaxColumns(line, model.columns))
            lineNumber += 1
        }
    }
}
