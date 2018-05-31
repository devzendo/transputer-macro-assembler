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

import org.devzendo.tma.util.TempFolder
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

import scala.io.Source

class TestListingWriter extends TempFolder with AssertionsForJUnit with MustMatchers {

    private val listingFile: File = File.createTempFile("out.", ".lst", temporaryDirectory)
    private val writer: ListingWriter = new ListingWriter(listingFile)

    private def lineAccess(op: LineAccess => Unit) = {
        val la = new LineAccess()
        op(la)
    }

    private class LineAccess {
        private val lines: Array[String] = Source.fromFile(listingFile).getLines().toArray

        def line(number: Int): String = {
            lines(number)
        }
        def numLines = lines.length
    }

}
