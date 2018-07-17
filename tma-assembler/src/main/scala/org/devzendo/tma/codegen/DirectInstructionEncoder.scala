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

import scala.collection.mutable.ListBuffer

object DirectInstructionEncoder {

    private val PFIX: Int = 0x20
    private val NFIX: Int = 0x60

    /*
     * Given a direct instruction with its opcode in its left-hand nybble, and a value for its arg, encode the
     * shortest sequence of (pfix|nfix)*<opcode> bytes that encode the direct instruction.
     *
     * Algorithm converted from "The Transputer Handbook", Graham & King, p24.
     */
    def apply(op: Int, arg: Int): List[Int] = {
        val prefixed = if (arg < 0) {
            encodeStep((~arg) >> 4, true)
        } else {
            if (arg > 15) {
                encodeStep(arg >> 4, false)
            } else {
                new ListBuffer[Int]()
            }
        }
        val oparg = op | (arg & 0x0f)
        prefixed += oparg
        prefixed.toList
    }

    private def encodeStep(arg: Int, negative: Boolean): ListBuffer[Int] = {
        if (arg > 15) {
            encodeStep(arg >> 4, negative) += (PFIX | (if (negative) {
                ~arg
            } else {
                arg
            }) & 0x0f)
        } else {
            val fix = if (negative) {
                NFIX
            } else {
                PFIX
            }
            new ListBuffer[Int]() += (fix | (arg & 0x0f))
        }
    }
}
