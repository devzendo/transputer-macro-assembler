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

package org.devzendo.tma.codegen

import org.devzendo.tma.ast._
import org.junit.rules.ExpectedException
import org.junit.{Rule, Test}
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestOffsetTransformer extends AssertionsForJUnit with MustMatchers {

    val fnord = "FNORD"

    @Rule
    def thrown: ExpectedException = _thrown
    var _thrown: ExpectedException = ExpectedException.none

    @Test
    def convertRepeatedOffsetsCountUndefined(): Unit = {
        thrown.expect(classOf[StatementTransformationException])
        thrown.expectMessage("Count of 'SymbolArg(FNORD)' is undefined")

        val count = SymbolArg(fnord)
        val repeatedExpr = Number(7)

        val inmodel = new AssemblyModel(true)
        new OffsetTransformer(inmodel).convertRepeatedOffsets(count, repeatedExpr, 1)
    }

    @Test
    def offsetConversionToOffsetFrom(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)
        val convertedExpr = new OffsetTransformer(inmodel).convertOffsets(Unary(Offset(), Number(0x10)))
        convertedExpr must be(Unary(OffsetFrom(0x1000), Number(0x10)))
    }

    @Test
    def nonOffsetExpressionInUnaryDoesNotGetConverted(): Unit = {
        val inmodel = new AssemblyModel(true)

        val convertedExpr = new OffsetTransformer(inmodel).convertOffsets(Unary(Negate(), Number(0x10)))
        convertedExpr must be(Unary(Negate(), Number(0x10)))
    }

    @Test
    def nonOffsetExpressionInNonUnaryDoesNotGetConverted(): Unit = {
        val inmodel = new AssemblyModel(true)

        val convertedExpr = new OffsetTransformer(inmodel).convertOffsets(Characters("foo"))
        convertedExpr must be(Characters("foo"))
    }

    @Test
    def listOfOffsetBytesGetIncreasingDollar(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)

        val convertedExpr = new OffsetTransformer(inmodel).convertListOfOffsets(List(
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10))), 1)
        convertedExpr must be(List(
            Unary(OffsetFrom(0x1000), Number(0x10)),
            Unary(OffsetFrom(0x1001), Number(0x10)),
            Unary(OffsetFrom(0x1002), Number(0x10))))
    }

    @Test
    def listOfOffsetWordsGetIncreasingDollar(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)

        val convertedExpr = new OffsetTransformer(inmodel).convertListOfOffsets(List(
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10))), 2)
        convertedExpr must be(List(
            Unary(OffsetFrom(0x1000), Number(0x10)),
            Unary(OffsetFrom(0x1002), Number(0x10)),
            Unary(OffsetFrom(0x1004), Number(0x10))))
    }

    @Test
    def listOfOffsetDoubleWordsGetIncreasingDollar(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)

        val convertedExpr = new OffsetTransformer(inmodel).convertListOfOffsets(List(
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10)),
            Unary(Offset(), Number(0x10))), 4)
        convertedExpr must be(List(
            Unary(OffsetFrom(0x1000), Number(0x10)),
            Unary(OffsetFrom(0x1004), Number(0x10)),
            Unary(OffsetFrom(0x1008), Number(0x10))))
    }

    @Test
    def listOfOffsetsInterspersedWithConstantsGetIncreasingDollar(): Unit = {
        val inmodel = new AssemblyModel(true)
        inmodel.setDollarSilently(0x1000)

        val convertedExpr = new OffsetTransformer(inmodel).convertListOfOffsets(List(
            Unary(Offset(), Number(0x10)),
            Number(0x10),
            Unary(Offset(), Number(0x10))), 4)
        convertedExpr must be(List(
            Unary(OffsetFrom(0x1000), Number(0x10)),
            Number(0x10),
            Unary(OffsetFrom(0x1008), Number(0x10))))
    }
}
