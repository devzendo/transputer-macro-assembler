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

import org.devzendo.tma.AssemblerFixture
import org.devzendo.tma.ast._
import org.devzendo.tma.output.ShowListingFixture
import org.junit.Test
import org.log4s.Logger
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.mutable.ArrayBuffer

class TestOffsetTransformerAssembly extends AssemblerFixture with SourcedValuesFixture with ShowListingFixture with AssertionsForJUnit with MustMatchers {
    val logger: Logger = org.log4s.getLogger


    private def dataForAssembledBranch(lines: String*): List[Int] = {
        val linesToParse: ArrayBuffer[String] = wrapInPrologueAndEpilogue(lines:_*)

        // Find the branch in the passed lines..
        val branchLineIndex = linesToParse.indexWhere(_.contains("J"))
        if (branchLineIndex == -1) {
            fail("No branch")
        } else {
            logger.debug("checking branch line index " + branchLineIndex)
            linesToParse.zipWithIndex.foreach { tuple: (String, Int) =>
                logger.debug("idx#" + tuple._2 + ": " + tuple._1)
            }
            val model = assemble(linesToParse.toList)
            //showListing(model)
            model.dump()

            val branchLineNumber = model.allLines()(branchLineIndex).location.lineNumber
            logger.debug("checking branch line number " + branchLineNumber)

            val branchLineSourcedValues = model.getSourcedValuesForLineIndex(branchLineIndex)
            logger.debug("branchLineSourcedValues: on line number " + branchLineNumber + ": " + branchLineSourcedValues)
            val branchLineStorage = singleStorage(branchLineSourcedValues)
            branchLineStorage.cellWidth must be(1)
            val dataBytes = branchLineStorage.data.toList
            logger.debug("branchLineSourcedValues: data " + dataBytes)
            logger.debug("model in the test is " + model)
            dataBytes
        }
    }

    private def wrapInPrologueAndEpilogue(lines: String*): ArrayBuffer[String] = {
        val linesToParse = new ArrayBuffer[String]()
        linesToParse ++= List(
            ".TRANSPUTER",
            "ORG 0x1000",
            "BEFORE:"
        )
        linesToParse ++= lines
        linesToParse ++= List(
            "AFTER:",
            "END"
        )
        linesToParse
    }

    @Test
    def linesInModelReceiveTheTransformedLines(): Unit = {
        val assembler = wrapInPrologueAndEpilogue(
            "DB 0",
            "J BEFORE")
        val model = assemble(assembler.toList)

        def itsACorrectlyTransformedLine(line: IndexedLine): Boolean = {
            line.stmt match {
                case Some(DirectInstruction("J", 0x00, Unary(OffsetFrom(0x1001), SymbolArg("BEFORE")))) =>
                    true
                case _ =>
                    false
            }
        }
        model.allLines().exists(itsACorrectlyTransformedLine)
    }


    @Test
    def branchOffsetMinus3(): Unit = {
        dataForAssembledBranch(
            // BEFORE is 1000
            /* 1000: */ "DB 0",
            /* 1001: */ "J BEFORE" // IPtr is 1003 and will have Oreg added to it.
        ) must be(List(0x60, 0x0D))
    }

    @Test
    def branchOffsetMinus2(): Unit = {
        dataForAssembledBranch(
            // BEFORE is 1000
            /* 1000: */ "J BEFORE" // IPtr is 1002 and will have Oreg added to it.
        ) must be(List(0x60, 0x0E))
    }

    // ...

    @Test
    def branchOffsetHere(): Unit = {
        dataForAssembledBranch(
            /* 1000: */ "HERE: J HERE" // IPtr is 1002 and will have Oreg added to it.
        ) must be(List(0x60, 0x0E))
    }

    // ...

    @Test
    def branchOffset0(): Unit = {
        dataForAssembledBranch(
            /* 1000: */ "J AFTER" // IPtr is 1001 and will have Oreg added to it.
            /* 1001: AFTER: */
        ) must be(List(0x00))
    }

    // ...

    @Test
    def branchOffset1(): Unit = {
        dataForAssembledBranch(
            /* 1000: */ "J AFTER", // IPtr is 1001; += Oreg
            /* 1001: */ "DB 0"
            /* 1002: AFTER: */
        ) must be(List(0x01))
    }

    @Test
    def branchOffset2(): Unit = {
        dataForAssembledBranch(
            "J AFTER",
            "DB 2 DUP(0)"
            // 1003: AFTER:
        ) must be(List(0x02))
    }

    @Test
    def branchOffset2NotDuped(): Unit = {
        dataForAssembledBranch(
            "J AFTER",
            "DB 0, 0"
        ) must be(List(0x02))
    }

    @Test
    def branchOffset3(): Unit = {
        dataForAssembledBranch(
            "J AFTER",
            "DB 3 DUP(0)"
        ) must be(List(0x03))
    }

    // ...

    @Test
    def branchOffset14(): Unit = {
        dataForAssembledBranch(
            "J AFTER",
            "DB 14 DUP(0)"
        ) must be(List(0x0E))
    }

    @Test
    def branchOffset15(): Unit = { // maximum single byte instruction sequence
        dataForAssembledBranch(
            "J AFTER",
            "DB 15 DUP(0)"
        ) must be(List(0x0F))
    }

    // ...

    @Test
    def branchOffsetTwoByteInstructionSequence(): Unit = {
        dataForAssembledBranch(
            "J AFTER",
            "DB 16 DUP(0)"
        ) must be(List(0x21, 0x00))
    }
}
