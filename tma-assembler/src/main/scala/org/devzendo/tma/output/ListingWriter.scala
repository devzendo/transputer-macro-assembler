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
import org.devzendo.tma.codegen.{AssemblyModel, AssignmentValue, SourcedValue, Storage}

object ListingWriter {
    def numPrintableLinesForSourcedValue(sourcedValue: SourcedValue): Int = {
        sourcedValue match {
            case storage: Storage =>
                if (storage.data.length == 0) {
                    1
                } else {
                    storage.cellWidth match {
                        case 1 => (storage.data.length + 4) / 5
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

        def calculatePrintableLines(): Int = {
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

        logger.info("Writing listing file " + outputFile.getName)
        val header = headerLeft + (" " * (model.columns - headerLeft.length - headerRight.length)) + headerRight

        var lineNumber = 0
        var pageNumber = 1

        val printableLines = calculatePrintableLines()
        val pageHeightWithoutHeader = model.rows - 2
        val maxPageNumber = (printableLines + pageHeightWithoutHeader - 1) / pageHeightWithoutHeader
        logger.debug(s"printable lines $printableLines maxPageNumber $maxPageNumber pageHeightWithoutHeader $pageHeightWithoutHeader")
        // TODO not doing symbol table dump yet, that'll start on a new page

        val printWriter = new PrintWriter(outputFile)
        try {
            model.foreachLineSourcedValues((line: Line, sourcedValues: List[SourcedValue]) => {
                logger.info(s"line $line")
                for (sv <- sourcedValues) {
                    sv match {
                        case st: Storage =>
                            logger.info(s"  data ${st.data.toList} storage $st")
                        case av: AssignmentValue =>
                            logger.info(s"  data ${av.data} assigned value $av")
                    }
                }

                val lineBuf = new StringBuilder()

                var address = ""
                var assignment = ""
                for (sourcedValue <- sourcedValues) {
                    logger.debug(s"sourcedValue $sourcedValue")
                    sourcedValue match {
                        case storage: Storage => address = HexDump.int2hex(storage.address)
                        case assignmentValue: AssignmentValue =>
                            if (assignmentValue.isLabel) {
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
                val left = padToLength(" " + List(address, assignment).filter(_.nonEmpty).mkString(" "), 21)
                logger.debug(s"address '$address' assignment '$assignment' left '$left'")
                lineBuf.append(left) // TODO storage db/dw/dd
                // TODO constant/variable assignment = values, going to need some model changes to store the value assigned on each line a la Storage
                lineBuf.append(line.text)

                emitLineWithHeader(lineBuf.toString())
                // TODO storage overflow data lines that won't fit in first 17 chars


            })

            padOutToFullPages()

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
