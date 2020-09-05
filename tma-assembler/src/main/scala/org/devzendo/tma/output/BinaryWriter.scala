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
import org.devzendo.tma.codegen.{AssemblyModel, Endianness, SourcedValue, Storage}
import org.log4s.Logger

class BinaryWriter(val outputFile: File) {
    val logger: Logger = org.log4s.getLogger
    logger.debug("Writing binary file " + outputFile.getAbsoluteFile)


    def encode(model: AssemblyModel): Unit = {
        logger.info("Writing binary file " + outputFile.getName)
        val start = model.lowestStorageAddress
        logger.info("Start address 0x" + HexDump.int2hex(start))
        val end = model.highestStorageAddress
        logger.info("End address 0x" + HexDump.int2hex(end))
        val fileSize = end - start
        val raf = new RandomAccessFile(outputFile, "rw")
        try {
            logger.debug("Zeroing 0x" + HexDump.int2hex(fileSize) + " byte(s)")
            zero(raf, fileSize)
            model.foreachSourcedValue((lineNumber: Int, sourcedValues: List[SourcedValue]) => {
                logger.debug("Writing storage for line " + lineNumber)
                for (sourcedValue <- sourcedValues) {
                    sourcedValue match {
                        case storage: Storage =>
                            val address = storage.address
                            val offset = address - start
                            logger.debug("Seeking to offset 0x" + HexDump.int2hex(offset) + " for address 0x" + HexDump.int2hex(address))
                            raf.seek(offset)

                            // Endianness encoding, yes you can use Channels... a bit of a faff
                            for (dataInt <- storage.data) {
                                val bytes = encodeData(dataInt, storage.cellWidth, model.endianness)
                                raf.write(bytes)
                            }
                        case _ => // do nothing
                    }
                }
            })
        } finally {
            raf.close()
        }
    }

    private def encodeData(dataInt: Int, cellWidth: Int, endianness: Endianness.Value): Array[Byte] = {
        cellWidth match {
            case 1 =>
                logger.debug("Encoding Byte 0x" + HexDump.byte2hex(dataInt.toByte))
                Array[Byte](dataInt.toByte)
            case 2 =>
                logger.debug("Encoding " + endianness + " Endian Word 0x" + HexDump.short2hex(dataInt.toShort))
                endianness match {
                    case Endianness.Little =>
                        Array[Byte](
                            ( dataInt & 0x00ff).toByte,
                            ((dataInt & 0xff00) >> 8).toByte
                        )
                    case Endianness.Big =>
                        Array[Byte](
                            ((dataInt & 0xff00) >> 8).toByte,
                            ( dataInt & 0x00ff).toByte
                        )
                }
            case 4 =>
                logger.debug("Encoding " + endianness + " Endian Double Word 0x" + HexDump.int2hex(dataInt))
                endianness match {
                    case Endianness.Little =>
                        Array[Byte](
                            ( dataInt & 0x000000ff).toByte,
                            ((dataInt & 0x0000ff00) >> 8).toByte,
                            ((dataInt & 0x00ff0000) >> 16).toByte,
                            ((dataInt & 0xff000000) >> 24).toByte
                        )
                    case Endianness.Big =>
                        Array[Byte](
                            ((dataInt & 0xff000000) >> 24).toByte,
                            ((dataInt & 0x00ff0000) >> 16).toByte,
                            ((dataInt & 0x0000ff00) >> 8).toByte,
                            ( dataInt & 0x000000ff).toByte
                        )
                }
        }
    }

    private def zero(raf: RandomAccessFile, fileSize: Int): Unit = {
        raf.seek(0)
        val emptyChunkSize = 128
        val emptiness = Array.fill[Byte](emptyChunkSize)(0)
        var remaining = fileSize
        var offset = 0
        while (remaining > 0) {
            val thisChunkSize = Math.min(emptyChunkSize, remaining)
            logger.debug("  Writing " + thisChunkSize + " zero(es) at offset 0x" + HexDump.int2hex(offset))
            raf.write(emptiness, 0, thisChunkSize)
            remaining = remaining - thisChunkSize
            offset = offset + thisChunkSize
        }
        raf.seek(0)
    }
}
