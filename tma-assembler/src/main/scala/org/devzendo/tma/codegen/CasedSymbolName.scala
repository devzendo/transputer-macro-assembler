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

case object CasedSymbolName {
    // "By default, MASM is not case sensitive." MASM 6.1 Programmer's Guide, p25.
    var caseSensitivity = false

    def setCaseSensitivity(newSensitivity: Boolean): Unit = {
        caseSensitivity = newSensitivity
    }

    def apply(oddCaseName: String): CasedSymbolName = {
        val name = if (CasedSymbolName.caseSensitivity) oddCaseName else oddCaseName.toUpperCase
        new CasedSymbolName(name)
    }
}

case class CasedSymbolName private (private val name: String) {
    override def toString: String = name
}
