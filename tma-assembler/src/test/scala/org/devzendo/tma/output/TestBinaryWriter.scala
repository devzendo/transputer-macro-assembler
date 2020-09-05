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

package org.devzendo.tma.output

import java.io.{File, RandomAccessFile}

import org.devzendo.commoncode.string.HexDump
import org.devzendo.tma.ast._
import org.devzendo.tma.codegen.{AssemblyModel, Endianness}
import org.devzendo.tma.util.TempFolder
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestBinaryWriter extends TempFolder with AssertionsForJUnit with MustMatchers {

    private val binaryFile: File = File.createTempFile("out.", ".bin", temporaryDirectory)
    private val writer: BinaryWriter = new BinaryWriter(binaryFile)

    private def byteAccess(op: ByteAccess => Unit) = {
        val ba = new ByteAccess()
        try {
            op(ba)
        } finally {
            ba.close()
        }
    }

    private class ByteAccess {
        val raf = new RandomAccessFile(binaryFile, "r")
        def byte(pos: Long) = {
            raf.seek(pos)
            raf.readByte()
        }
        def close(): Unit = {
            raf.close()
        }
    }

    private def dumpFile() = {
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

    private def genDummyLine(lineNumber: Int) = Line(lineNumber, "", None, None)

    @Test
    def fileSizeAndContents(): Unit = {
        val model = new AssemblyModel(true)

        model.setDollar(0x4000, genDummyLine(1))

        val exprs1 = List(Number(1), Number(2))
        model.allocateStorageForLine(Line(2, "", None, Some(DB(exprs1))), 1, exprs1)

        model.setDollar(0x4020, genDummyLine(3))

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
    def originAtZeroContents(): Unit = {
        val model = new AssemblyModel(true)

        val exprs1 = List(Number(1), Number(2))
        model.allocateStorageForLine(Line(2, "", None, Some(DB(exprs1))), 1, exprs1)

        val size = model.highestStorageAddress - model.lowestStorageAddress
        size must be(0x01)

        writer.encode(model)

        dumpFile()

        binaryFile.length() must be(0x02)

        byteAccess(ba => {
            ba.byte(0x0000) must be(1)
            ba.byte(0x0001) must be(2)
        })
    }

    @Test
    def overlappingStorage(): Unit = {
        val model = new AssemblyModel(true)

        model.setDollar(0x4000, genDummyLine(1))

        val exprs1 = List(Number(1), Number(2), Number(3), Number(4))
        model.allocateStorageForLine(Line(2, "", None, Some(DB(exprs1))), 1, exprs1)

        model.setDollar(0x4001, genDummyLine(3))

        val exprs2 = List(Number(5), Number(6))
        model.allocateStorageForLine(Line(4, "", None, Some(DB(exprs2))), 1, exprs2)

        val size = model.highestStorageAddress - model.lowestStorageAddress
        size must be(0x03)

        writer.encode(model)

        dumpFile()

        binaryFile.length() must be(0x04)

        byteAccess(ba => {
            ba.byte(0x0000) must be(1)
            ba.byte(0x0001) must be(5)
            ba.byte(0x0002) must be(6)
            ba.byte(0x0003) must be(4)
        })
    }

    @Test
    def littleEndianWord(): Unit = {
        val model = new AssemblyModel(true)

        model.endianness = Endianness.Little

        val words = List(Number(0x1234), Number(0x5678), Number(0x9ABC))
        model.allocateStorageForLine(Line(1, "", None, Some(new DW(words))), 2, words)

        val size = model.highestStorageAddress - model.lowestStorageAddress
        size must be(0x05)

        writer.encode(model)

        dumpFile()

        binaryFile.length() must be(0x06)

        byteAccess(ba => {
            ba.byte(0x0000) must be(0x34)
            ba.byte(0x0001) must be(0x12)

            ba.byte(0x0002) must be(0x78)
            ba.byte(0x0003) must be(0x56)

            ba.byte(0x0004) must be(0xBC.toByte) // stupid JVM unsigneds..
            ba.byte(0x0005) must be(0x9A.toByte)
        })
    }

    @Test
    def bigEndianWord(): Unit = {
        val model = new AssemblyModel(true)

        model.endianness = Endianness.Big

        val words = List(Number(0x1234), Number(0x5678), Number(0x9ABC))
        model.allocateStorageForLine(Line(1, "", None, Some(new DW(words))), 2, words)

        val size = model.highestStorageAddress - model.lowestStorageAddress
        size must be(0x05)

        writer.encode(model)

        dumpFile()

        binaryFile.length() must be(0x06)

        byteAccess(ba => {
            ba.byte(0x0000) must be(0x12)
            ba.byte(0x0001) must be(0x34)

            ba.byte(0x0002) must be(0x56)
            ba.byte(0x0003) must be(0x78)

            ba.byte(0x0004) must be(0x9A.toByte) // stupid JVM unsigneds..
            ba.byte(0x0005) must be(0xBC.toByte)
        })
    }

    @Test
    def littleEndianDoubleWord(): Unit = {
        val model = new AssemblyModel(true)

        model.endianness = Endianness.Little

        val words = List(Number(0x01234567), Number(0x89ABCDEF))
        model.allocateStorageForLine(Line(1, "", None, Some(new DD(words))), 4, words)

        val size = model.highestStorageAddress - model.lowestStorageAddress
        size must be(0x07)

        writer.encode(model)

        dumpFile()

        binaryFile.length() must be(0x08)

        byteAccess(ba => {
            ba.byte(0x0000) must be(0x67)
            ba.byte(0x0001) must be(0x45)
            ba.byte(0x0002) must be(0x23)
            ba.byte(0x0003) must be(0x01)

            ba.byte(0x0004) must be(0xEF.toByte) // stupid JVM unsigneds..
            ba.byte(0x0005) must be(0xCD.toByte)
            ba.byte(0x0006) must be(0xAB.toByte)
            ba.byte(0x0007) must be(0x89.toByte)
        })
    }

    @Test
    def bigEndianDoubleWord(): Unit = {
        val model = new AssemblyModel(true)

        model.endianness = Endianness.Big

        val words = List(Number(0x01234567), Number(0x89ABCDEF))
        model.allocateStorageForLine(Line(1, "", None, Some(new DD(words))), 4, words)

        val size = model.highestStorageAddress - model.lowestStorageAddress
        size must be(0x07)

        writer.encode(model)

        dumpFile()

        binaryFile.length() must be(0x08)

        byteAccess(ba => {
            ba.byte(0x0000) must be(0x01)
            ba.byte(0x0001) must be(0x23)
            ba.byte(0x0002) must be(0x45)
            ba.byte(0x0003) must be(0x67)

            ba.byte(0x0004) must be(0x89.toByte) // stupid JVM unsigneds..
            ba.byte(0x0005) must be(0xAB.toByte)
            ba.byte(0x0006) must be(0xCD.toByte)
            ba.byte(0x0007) must be(0xEF.toByte)
        })
    }
}
