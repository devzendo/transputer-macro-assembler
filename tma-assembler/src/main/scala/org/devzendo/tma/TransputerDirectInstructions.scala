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

trait TransputerDirectInstructions {
    val OP_J = 0x00
    val OP_LDLP = 0x10
    val OP_PFIX = 0x20
    val OP_LDNL = 0x30
    val OP_LDC = 0x40
    val OP_LDNLP = 0x50
    val OP_NFIX = 0x60
    val OP_LDL = 0x70
    val OP_ADC = 0x80
    val OP_CALL = 0x90
    val OP_CJ = 0xa0
    val OP_AJW = 0xb0
    val OP_EQC = 0xc0
    val OP_STL = 0xd0
    val OP_STNL = 0xe0
    val OP_OPR = 0xf0
}
