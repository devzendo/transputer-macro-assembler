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

import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit

class TestDirectInstructionEncoder extends AssertionsForJUnit with MustMatchers {
    import DirectInstructionEncoder._
    val logger = org.log4s.getLogger

    private val LDC = 0x40 // Other direct instructions are available; they all encode in the same way.

    def ldc(arg: Int): List[Int] = {
        val ints = apply(LDC, arg)
        val intsHex = ints map { i: Int => f"0x$i%02x" } mkString " "
        logger.info(f"Arg 0x$arg%08x: $intsHex%s")
        logger.info(f"Arg $arg%d: $ints%s")
        ints
    }

    @Test
    def positiveNybble(): Unit = {
        ldc(0x0c) must be (List(0x4c))
    }

    @Test
    def positiveNybbleMaxSingleByteInstructionSequence(): Unit = {
        ldc(0x0f) must be (List(0x4f))
    }

    @Test
    def positiveNybbleFirstDoubleByteInstructionSequence(): Unit = {
        ldc(0x10) must be (List(0x21, 0x40))
    }

    @Test
    def positiveNybbleSecondDoubleByteInstructionSequence(): Unit = {
        ldc(0x11) must be (List(0x21, 0x41))
    }

    @Test
    def positiveByte(): Unit = {
        ldc(0x28) must be (List(0x22, 0x48))
    }

    @Test
    def positiveWord(): Unit = {
        ldc(257) must be (List(0x21, 0x20, 0x41))
    }

    @Test
    def positiveDWord(): Unit = {
        ldc(0x1234abcd) must be (List(0x21, 0x22, 0x23, 0x24, 0x2a, 0x2b, 0x2c, 0x4d))
    }

    @Test
    def positiveMaxDWord(): Unit = {
        ldc(0x7FFFFFFF) must be (List(0x27, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x4f))
    }

    @Test
    def negativeNybbleMinusTwo(): Unit = {
        ldc(-2) must be (List(0x60, 0x4e))
    }

    @Test
    def negativeNybbleMinusOne(): Unit = {
        ldc(-1) must be (List(0x60, 0x4f))
    }

    @Test
    def negativeNybble(): Unit = {
        ldc(-0x0c) must be (List(0x60, 0x44))
    }

    @Test
    def negativeByte(): Unit = {
        ldc(-0x28) must be (List(0x62, 0x48))
    }

    @Test
    def negativeWord(): Unit = {
        ldc(-257) must be (List(0x61, 0x2f, 0x4f))
    }

    @Test
    def negativeMaxDWord(): Unit = {
        ldc(0x80000000) must be (List(0x67, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x40))
    }
}
