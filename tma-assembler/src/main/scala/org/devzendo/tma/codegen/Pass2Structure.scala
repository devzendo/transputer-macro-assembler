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

package org.devzendo.tma.codegen

import org.devzendo.tma.ast.{Line, Statement}

import scala.collection.mutable

/**
  * State store for Pass 2 addresses, lines/statements to be assembled in Pass 2, and the size of the block assembled
  * in Pass 1. Created in If1/Else/Endif processing.
  */
class Pass2Structure {

    private var startAddress: Int = 0
    private var endAddress: Int = 0

    // The Int here is a line index (could refer to expanded macros) not a line number (refer to input source lines,
    // and is available in line.number).
    private val linesAndIndices = mutable.ArrayBuffer[(Line, Int)]()

    def setStartAddress(startAddress: Int): Unit = {
        this.startAddress = startAddress
    }
    def getStartAddress = startAddress

    def setEndAddress(endAddress: Int): Unit = {
        this.endAddress = endAddress
    }
    def getEndAddress = endAddress

    def getPass1BlockSize = endAddress - startAddress

    def addPass2Line(tuple: (Line, Int)): Unit = {
        linesAndIndices += tuple
    }
    def getPass2Lines: List[(Line, Int)] = linesAndIndices.toList
}
