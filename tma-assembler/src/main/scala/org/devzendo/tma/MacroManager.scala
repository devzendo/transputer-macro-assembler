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

import org.apache.commons.lang.StringUtils
import org.devzendo.tma.ast.AST.{MacroArgument, MacroName, MacroParameterName}
import org.devzendo.tma.ast.MacroDefinition
import org.log4s.Logger

import scala.collection.mutable

class MacroManager {
    val logger: Logger = org.log4s.getLogger

    private val macros = mutable.Map[MacroName, MacroDefinition]()

    private var inMacroBody = false


    def isInMacroBody: Boolean = inMacroBody

    def getMacro(macroName: MacroName): Option[MacroDefinition] = macros.get(macroName)

    def exists(macroName: MacroName): Boolean = macros.contains(macroName)

    def storeMacro(macroName: MacroName, definition: MacroDefinition): Unit = macros(macroName) = definition


    private var macroName: MacroName = _
    private var macroParameterNames: List[MacroParameterName] = List.empty
    def startMacro(macroName: MacroName, macroParameterNames: List[MacroParameterName]): Unit = {
        if (macros.contains(macroName)) {
            throw new IllegalStateException("Macro '" + macroName + "' already defined")
        }
        inMacroBody = true
        this.macroName = macroName
        this.macroParameterNames = macroParameterNames
        macroLines.clear()
    }

    private val macroLines = mutable.ArrayBuffer[String]()
    def addMacroLine(line: String): Unit = {
        if (!isInMacroBody) {
            throw new IllegalStateException("Macro line received with no start macro")
        }
        macroLines += line
    }

    def endMacro(): Unit = {
        if (!isInMacroBody) {
            throw new IllegalStateException("End macro with no start macro")
        }
        inMacroBody = false
        macros(macroName) = MacroDefinition(macroName, macroParameterNames, macroLines.toList)
    }


    def expandMacro(macroName: MacroName, arguments: List[MacroArgument]): List[String] = {
        logger.debug("expandMacro(" + macroName + ", " + arguments + ")")
        val defn = macros.getOrElse(macroName, {
            throw new IllegalStateException("Macro '" + macroName + "' does not exist")
        })
        if (arguments.length > defn.parameterNames.length) {
            throw new IllegalStateException("Macro '" + macroName + "' has " + defn.parameterNames.length + " parameters, but is called with " + arguments.length)
        }

        // Map parameter names to arguments, or empty strings if the argument list is shorter than the parameter list.
        def getArg(i: Int): MacroArgument = {
            val emptyArg = new MacroArgument("")
            if (i < arguments.length) {
                arguments(i)
            } else {
                emptyArg
            }
        }
        val paramAndIndex = defn.parameterNames.zipWithIndex
        for (elem <- paramAndIndex) { logger.debug("param #" + elem._2 + "=" + elem._1) }
        val paramToArgMap = paramAndIndex.foldLeft(Map[MacroParameterName, MacroArgument]()) {
            (m, pi) => m + (pi._1 -> getArg(pi._2))
        }
        for (elem <- paramAndIndex) { logger.debug("  arg #" + elem._2 + "=" + paramToArgMap(elem._1)) }

        // Yuk, mutability!
        def expandLine(instr: String): String = {
            var str = instr
            defn.parameterNames.foreach( pn => str = StringUtils.replace(str, pn, paramToArgMap(pn)))
            str
        }
        val expansion = defn.textLines map { expandLine }
        for (lineAndNumber <- expansion.zipWithIndex) { logger.debug("line: #" + lineAndNumber._2 + "=|" + lineAndNumber._1 + "|")}
        expansion
    }
}
