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

import org.scalatest.MustMatchers
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class TestCasedSymbolName extends AssertionsForJUnit with MustMatchers {

    // The default is insensitive, i.e. CasedSymbolName.caseSensitivity must be(false)
    // but since this setting is object-based not an instance variable, other test execution will set this as needed,
    // so it can't reliably be sensed in a test. It is set only once in the assembler's main, from the command line.

    // As a consequence, any test that sets it true must set it back to false, to correctly isolate other tests.

    @Test
    def insensitive(): Unit = {
        CasedSymbolName.setCaseSensitivity(false)

        val casedSymbolName = CasedSymbolName("fNoRd")
        casedSymbolName.toString must be("FNORD")
    }

    @Test
    def sensitive(): Unit = {
        CasedSymbolName.setCaseSensitivity(true)

        try {
            val casedSymbolName = CasedSymbolName("fNoRd")
            casedSymbolName.toString must be("fNoRd")
        } finally {
            CasedSymbolName.setCaseSensitivity(false)
        }
    }
}
