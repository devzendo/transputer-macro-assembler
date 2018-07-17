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

import org.devzendo.tma.ast.Statement

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
 * This algorithm is the only one which is guaranteed to converge.
 */
class DirectInstructionOffsetEncoder(val model: AssemblyModel) {

    val statements = mutable.ArrayBuffer[Statement]()
    val symbolsToBeResolved = mutable.HashSet[String]()


    def addStatement(statement: Statement): Unit = {
        statements += statement
    }

    /**
      * Have all undefined symbols in all the supplied Statements been potentially resolved?
      * @return true iff all resolved
      */
    def isResolvable(): Boolean = {
        symbolsToBeResolved.isEmpty
    }

    def resolvedStatements(): List[Statement] = {
        statements.toList
    }
}
