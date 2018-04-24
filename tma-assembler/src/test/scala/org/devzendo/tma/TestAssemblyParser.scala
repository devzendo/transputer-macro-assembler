/*
 * Copyright (C) 2008-2017 Matt Gumbley, DevZendo.org http://devzendo.org
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
import org.devzendo.tma.ast.Line
import org.hamcrest.{MatcherAssert, Matchers}
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}

import collection.JavaConverters._
import org.mockito.ArgumentMatchers.any
import org.mockito.{ArgumentMatchers, InOrder, Mock, Mockito}
import org.mockito.Mockito.{never, times, verify, when}
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

class TestAssemblyParser extends AssertionsForJUnit with MustMatchers with MockitoSugar {
    val logger = org.log4s.getLogger

    val parser = new AssemblyParser(true)
    var lineNumber = 1

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    @Test
    def initial(): Unit = {
        parser.getLines() must be(empty)
    }

    @Test
    def positiveLineNumber(): Unit = {
        thrown.expect(classOf[AssemblyParserException])
        thrown.expectMessage("Line numbers must be positive")
        parser.parse((null, 0))
    }

    @Test
    def nullLine(): Unit = {
        parseLine(null)
        val lines = parser.getLines()
        lines must have size 1
        lines.head must equal(Line(1, "", List.empty, None, None, None))
    }

    @Test
    def emptyLine(): Unit = {
        parseLine("")
        val lines = parser.getLines()
        lines must have size 1
        lines.head must equal(Line(1, "", List.empty, None, None, None))
    }

    private def parseLine(line: String) = {
        parser.parse((line, lineNumber))
        lineNumber = lineNumber + 1
    }

    @Test
    def justAComment(): Unit = {
        parseLine("  ; comment  ")
        val lines = parser.getLines()
        lines must have size 1
        lines.head must equal(Line(1, "; comment", List.empty, None, None, None))
    }

}
