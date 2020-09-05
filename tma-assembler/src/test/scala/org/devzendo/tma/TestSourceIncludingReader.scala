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

import java.io.File

import org.devzendo.tma.util.TempFolder
import org.junit.{Ignore, Rule, Test}
import org.junit.rules.ExpectedException
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit
import org.log4s.Logger

class TestSourceIncludingReader extends TempFolder with AssertionsForJUnit with MustMatchers {
    val sourceFile: File = File.createTempFile("temp.", ".asm", temporaryDirectory)
    val sourceFileName: String = sourceFile.getName
    val reader = new SourceIncludingReader()

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    @Test
    def emptyFileYieldsEmptyIterator(): Unit = {
        val iterator = reader.openSourceIterator(sourceFile)
        iterator.hasNext must be(false)
    }

    @Test
    def populatedFileYieldsCorrectIteratorBehaviour(): Unit = {
        writeLinesToFile(sourceFile, "one", "two", "three")
        val iterator = reader.openSourceIterator(sourceFile)

        iterator.hasNext must be(true)
        iterator.next() must be(SourceItem(null, sourceFileName, 1, "one"))

        iterator.hasNext must be(true)
        iterator.next() must be(SourceItem(null, sourceFileName, 2, "two"))

        iterator.hasNext must be(true)
        iterator.next() must be(SourceItem(null, sourceFileName, 3, "three"))

        iterator.hasNext must be(false)
    }

    private def writeLinesToFile(file: File, lines: String*) = {
        val linesWithNL = lines.toList.map(_ + "\n").toArray
        reflect.io.File(file.getPath).writeAll(linesWithNL.toArray: _*)
    }
}
