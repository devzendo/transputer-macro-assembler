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

import java.util.regex.Pattern

import org.devzendo.tma.ast.AST.{MacroArgument, MacroName, MacroParameterName}
import org.devzendo.tma.ast.MacroDefinition
import org.log4s.Logger

import scala.collection.mutable

class MacroManager(val debugParser: Boolean) {
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
        val numUniqueParameterNames = macroParameterNames.toSet.size
        val numParameterNames = macroParameterNames.size
        if (numParameterNames != 0 && numParameterNames != numUniqueParameterNames) {
            throw new IllegalStateException("Macro '" + macroName + "' has duplicated parameter names")
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

    val delimCharsBefore = """(?<=[,.<>/?;:{}\[\]|!@#$%^&*()\-+=\s])"""
    val delimCharsAfter = """(?=[,.<>/?;:{}\[\]|!@#$%^&*()\-+=\s])"""
    def formMatchPattern(name: MacroParameterName): Pattern = {
        val nameAtStart = "^" + name + delimCharsAfter
        val nameBetweenDelimiters = delimCharsBefore + name + delimCharsAfter
        val nameAtEnd = delimCharsBefore + name + "$"
        val nameIsEntireString = "^" + name + "$"
        val regex = "(" + nameAtStart + "|" + nameBetweenDelimiters + "|" + nameAtEnd + "|" + nameIsEntireString + ")"
        Pattern.compile(regex)
    }

    def expandMacro(macroName: MacroName, arguments: List[MacroArgument]): List[String] = {
        if (debugParser) logger.debug("expandMacro(" + macroName + ", " + arguments + ")")
        val definition = macros.getOrElse(macroName, {
            throw new IllegalStateException("Macro '" + macroName + "' does not exist")
        })
        if (arguments.length > definition.parameterNames.length) {
            throw new IllegalStateException("Macro '" + macroName + "' has " + definition.parameterNames.length + " parameters, but is called with " + arguments.length)
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
        val paramAndIndex = definition.parameterNames.zipWithIndex
        val paramToArgMap = paramAndIndex.foldLeft(Map[MacroParameterName, MacroArgument]()) {
            (m, pi) => m + (pi._1 -> getArg(pi._2))
        }
        val paramNameToPattern = paramAndIndex.foldLeft(Map[MacroParameterName, Pattern]()) {
            (m, pi) => m + (pi._1 -> formMatchPattern(pi._1))
        }
        if (debugParser) for (elem <- paramAndIndex) { logger.debug("  arg #" + elem._2 + " " + elem._1 + "=" + paramToArgMap(elem._1)) }

        // Yuk, mutability!
        def expandLine(instr: String): String = {
            var str = instr
            definition.parameterNames.foreach( parameterName => str = paramNameToPattern(parameterName).matcher(str).replaceAll(paramToArgMap(parameterName)))

            //  definition.parameterNames.foreach( parameterName => str = StringUtils.replace(str, parameterName, paramToArgMap(parameterName)))
            str
        }
        val expansion = definition.textLines map { expandLine }
        if (debugParser) for (lineAndNumber <- expansion.zipWithIndex) { logger.debug("line: #" + lineAndNumber._2 + "=|" + lineAndNumber._1 + "|")}
        expansion
    }
}
