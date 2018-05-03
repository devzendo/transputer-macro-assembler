/*
 * Copyright (C) 2008-2017 Matt Gumbley, DevZendo.org http://devzendo.org
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

import org.devzendo.tma.ast.AST.{MacroArgName, MacroName}
import org.devzendo.tma.ast.MacroDefinition

import scala.collection.mutable

class MacroManager {


    private val macros = mutable.Map[MacroName, MacroDefinition]()

    private var inMacroBody = false

    def isInMacroBody: Boolean = inMacroBody


    def getMacro(macroName: MacroName): Option[MacroDefinition] = macros.get(macroName)

    def exists(macroName: MacroName) = macros.contains(macroName)

    def storeMacro(macroName: MacroName, definition: MacroDefinition) = macros(macroName) = definition



    private var macroName: MacroName = _
    private var macroArgNames: List[MacroArgName] = List.empty
    def startMacro(macroName: MacroName, macroArgNames: List[MacroArgName]) = {
        if (macros.contains(macroName)) {
            throw new IllegalStateException("Macro '" + macroName + "' already defined")
        }
        inMacroBody = true
        this.macroName = macroName
        this.macroArgNames = macroArgNames
        macroLines.clear()
    }

    private val macroLines = mutable.ArrayBuffer[String]()
    def addMacroLine(line: String) = {
        if (!isInMacroBody) {
            throw new IllegalStateException("Macro line received with no start macro")
        }
        macroLines += line
    }

    def endMacro() = {
        if (!isInMacroBody) {
            throw new IllegalStateException("End macro with no start macro")
        }
        inMacroBody = false
        macros(macroName) = new MacroDefinition(macroName, macroArgNames, macroLines.toList)
    }
}
