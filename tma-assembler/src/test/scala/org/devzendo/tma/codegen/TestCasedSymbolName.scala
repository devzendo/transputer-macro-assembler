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

    @Test
    def default(): Unit = {
        CasedSymbolName("fNoRd").name must be("FNORD")
    }

    @Test
    def insensitive(): Unit = {
        CasedSymbolName.setCaseSensitivity(false)

        CasedSymbolName("fNoRd").name must be("FNORD")
    }

    @Test
    def sensitive(): Unit = {
        CasedSymbolName.setCaseSensitivity(true)

        CasedSymbolName("fNoRd").name must be("fNoRd")
    }
}
