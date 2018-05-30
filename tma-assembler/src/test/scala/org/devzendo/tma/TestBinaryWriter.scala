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

import java.io.{File, RandomAccessFile}

import org.devzendo.commoncode.string.HexDump
import org.devzendo.tma.ast.{DB, Line, Number}
import org.devzendo.tma.codegen.AssemblyModel
import org.devzendo.tma.output.BinaryWriter
import org.devzendo.tma.util.TempFolder
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestBinaryWriter extends TempFolder with AssertionsForJUnit with MustMatchers {

    private val binaryFile: File = File.createTempFile("out.", ".bin", temporaryDirectory)
    private val writer: BinaryWriter = new BinaryWriter(binaryFile)

    def byteAccess(op: ByteAccess => Unit) = {
        val ba = new ByteAccess()
        try {
            op(ba)
        } finally {
            ba.close()
        }
    }

    class ByteAccess {
        val raf = new RandomAccessFile(binaryFile, "r")
        def byte(pos: Long) = {
            raf.seek(pos)
            raf.readByte()
        }
        def close(): Unit = {
            raf.close()
        }
    }

    def dumpFile() = {
        val raf = new RandomAccessFile(binaryFile, "r")
        try {
            val buffer = Array.fill[Byte](raf.length().toInt)(0)
            raf.readFully(buffer)
            for (string <- HexDump.hexDump(buffer)) {
                logger.debug(string)
            }
        } finally {
            raf.close()
        }
    }

    @Test
    def fileSizeAndContents(): Unit = {
        val model = new AssemblyModel()

        model.setDollar(0x4000, 1)

        val exprs1 = List(Number(1), Number(2))
        model.allocateStorageForLine(Line(2, "", None, Some(DB(exprs1))), 1, exprs1)

        model.setDollar(0x4020, 3)

        val exprs2 = List(Number(3), Number(4))
        model.allocateStorageForLine(Line(4, "", None, Some(DB(exprs2))), 1, exprs2)

        val size = model.highestStorageAddress - model.lowestStorageAddress
        size must be(0x21)

        writer.encode(model)

        dumpFile()

        binaryFile.length() must be(0x22)

        byteAccess(ba => {
            ba.byte(0x0000) must be(1)
            ba.byte(0x0001) must be(2)
            // everything else is zeroed out
            for (i <- 0x0002 to 0x001F) {
                ba.byte(i) must be(0)
            }
            ba.byte(0x0020) must be(3)
            ba.byte(0x0021) must be(4)
        })
    }

    @Test
    def overlappingStorage(): Unit = {
        val model = new AssemblyModel()

        model.setDollar(0x4000, 1)

        val exprs1 = List(Number(1), Number(2))
        model.allocateStorageForLine(Line(2, "", None, Some(DB(exprs1))), 1, exprs1)

        model.setDollar(0x4000, 3)

        val exprs2 = List(Number(3), Number(4))
        model.allocateStorageForLine(Line(4, "", None, Some(DB(exprs2))), 1, exprs2)

        val size = model.highestStorageAddress - model.lowestStorageAddress
        size must be(0x01)

        writer.encode(model)

        dumpFile()

        binaryFile.length() must be(0x02)

        byteAccess(ba => {
            ba.byte(0x0000) must be(3)
            ba.byte(0x0001) must be(4)
        })
    }
}
