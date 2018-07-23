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

import org.devzendo.tma.ast.{Line, Statement}

import scala.collection.mutable

/*
 * Starting with a DirectInstruction containing one or more undefined symbols, and all subsequent Statements until
 * these symbols are defined, compute the minimal encodings of all DirectInstructions in the list, converting them into
 * DirectEncodedInstructions. The resulting list is then available to the CodeGenerator.
 * The list of subsequent Statements may contain further undefined symbols - the CodeGenerator will keep giving
 * Statements to this encoder until all undefined symbols have potential resolutions. (The isResolvable() method is
 * used to inform it of this).
 *
 * The algorithm used here is from The Transputer Handbook, Graham & King, p48,49:
 *
 * "The solution is reasonably simple but time consuming. A data structure representing the whole program is built in
 * memory. Fixed length sections of code can be held as binary, but any label must be kept as a pointer to the label and
 * an associated size. Initially all offsets are assumed to fit in one nibble, with no prefixes needed. A pass over the
 * program is made, altering all those that require a larger offset to a suitable value. A further pass is then made,
 * expanding those instructions that do not now fit because the previous pass expanded instructions. This process
 * continues until no more changes need to be made.
 * This algorithm is the only one which is guaranteed to converge."
 *
 * The "data structure representing the whole program" is the AssemblyModel, in this case a StackedAssemblyModel which
 * reads through to the main AssemblyModel but caches writes. This allows this encoder to adjust labels/constants/
 * variables as necessary as it processes.
 *
 * need a DirectInstructionTrial that stores the length of its current encoding, and that can be asked if, when
 * evaluated, it will fit in that length. if not, it can have its encoding length increased by one byte. (then the loop
 * iterates). when all DITs return true, we're good, and can convert them to DirectEncodedInstructions
 * Each time round the loop, the StackedAssemblyModel's cached state is reset.
 *
 * DirectInstruction (from the Parser) -> (replaced by, for this encoder) DirectInstructionTrial -> (replaced when done) DirectEncodedInstruction
 *
 * DirectEncodedInstructions are then trivially generated into binary.
 *
 * This encoder could be part of the CodeGenerator - perhaps doesn't need to be a Strategy on a stack.. could just mark
 * the first/last line of the code needing encoding, or the index into the CodeGenerator's list of lines?
 */
class DirectInstructionOffsetEncoder(val model: AssemblyModel) {

    val lines = mutable.ArrayBuffer[Line]()
    val symbolsToBeResolved = mutable.HashSet[String]()

    /**
      * Add a line to the list of resolvable lines. If this is the first line, any label it contains will have been
      * added to the main AssemblyModel (which will later cause duplication)
      * @param line
      */
    def addLine(line: Line): Unit = {
        lines += line
    }

    /**
      * Have all undefined symbols in all the supplied Statements been potentially resolved?
      * @return true iff all resolved
      */
    def isResolvable(): Boolean = {
        symbolsToBeResolved.isEmpty
    }

    def resolvedLines(): List[Line] = {
        lines.toList
    }
}
