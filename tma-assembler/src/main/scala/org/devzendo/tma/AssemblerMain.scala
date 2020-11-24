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

package org.devzendo.tma

import java.io.File

import org.devzendo.tma.codegen.{AssemblyModel, CasedSymbolName, CodeGenerationException, CodeGenerator, OffsetTransformer}
import org.devzendo.tma.parser.{AssemblyParser, AssemblyParserException, MacroManager}

import scala.collection.mutable.ArrayBuffer

class AssemblerMain(val argList: List[String]) {
    private val logger = org.log4s.getLogger

    var argIndex = 0
    var asmFile: Option[File] = None
    var outputFile: Option[File] = None
    var binaryFile: Option[File] = None
    var listingFile: Option[File] = None
    var includePaths: ArrayBuffer[File] = new ArrayBuffer[File]()
    var debugParser = false
    var debugExpansion = false
    var showParserOutput = false
    var debugCodegen = false

    def existingFile(fileType: String, f: String): Option[File] = {
        val file = new File(f)
        if (file.exists()) {
            Some(file)
        } else {
            logger.error(s"The $fileType file '$f' does not exist")
            quit()
            None // won't get here
        }
    }

    while (argIndex < argList.length)
    {
        val f = argList(argIndex)

        def expectFileName(): Option[File] = {
            if (argIndex == argList.length - 1) {
                logger.error(s"$f requires a file as its argument")
                quit()
                None // won't get here
            }
            val ret = Some(new File(argList(argIndex + 1)))
            argIndex += 1
            ret
        }

        logger.debug("ARG: [" + f + "]")
        f match {
            case "--help" => usage(); exit()
            case "-?" => usage(); exit()
            case "--version"  => version(); exit()
            case "-p" | "--parser"  => debugParser = true
            case "-e" | "--expansion"  => debugExpansion = true
            case "-P" | "--parserOutput"  => showParserOutput = true
            case "-c" | "--codegen"  => debugCodegen = true
            case "-x" | "--caseSensitive"  => CasedSymbolName.setCaseSensitivity(true)

            case "-o" | "--output" =>
                outputFile = expectFileName()

            case "-b" | "--binary" =>
                binaryFile = expectFileName()

            case "-l" | "--listing" =>
                listingFile = expectFileName()

            case "-I" | "--includepath" =>
                val maybeIncludePath = expectFileName()
                val includePath = maybeIncludePath.head
                if (includePath.exists()) {
                    if (includePath.isDirectory) {
                        includePaths += includePath
                    } else {
                        logger.error(s"Include path '" + includePath.toString + "' is not a directory")
                        quit()
                    }
                } else {
                    logger.error(s"Include path '" + includePath.toString + "' does not exist")
                    quit()
                }

            case _ =>
                if (f.startsWith("-")) {
                    logger.error(s"Unknown command line option: '$f'")
                    logger.error("")
                    usage()
                    quit()
                }
                asmFile = existingFile("assembly", f)
        }

        argIndex += 1
    }

    if (asmFile.isEmpty) {
        errorQuit("An assembly file must be specified")
    }
    if (outputFile.isEmpty && binaryFile.isEmpty) {
        errorQuit("No output or binary output specified")
    }

    def start(): Unit = {
        val macroManager = new MacroManager(debugExpansion)
        val includer = new SourceIncludingReader
        includePaths.foreach(includer.addIncludePath)
        val parser = new AssemblyParser(debugParser, showParserOutput, macroManager, includer)
        val inmodel = new AssemblyModel(debugCodegen)
        val codegen = new CodeGenerator(debugCodegen, inmodel)
        codegen.addStatementTransformer(new OffsetTransformer(inmodel).transform)
        val controller = new AssemblerController(includer, parser, codegen)
        val asm = asmFile.get

        logger.debug(s"Start of parsing from from ${asm.getName}")
        val startParseTime = System.currentTimeMillis()
        controller.parseFile(asm)
        val endParseTime = System.currentTimeMillis()
        logger.debug(s"Parsing complete in ${endParseTime - startParseTime} ms")

        val parserExceptions = controller.getParseExceptions
        if (parserExceptions.nonEmpty) {
            logger.error("Parse errors:")
            parserExceptions.foreach( (f: AssemblyParserException) => logger.error(f.getMessage))
            errorQuit("Cannot continue")
        }

        val startCodegenTime = System.currentTimeMillis()
        val model = controller.generateModel()
        val endCodegenTime = System.currentTimeMillis()
        logger.debug(s"Code generation complete in ${endCodegenTime - startCodegenTime} ms")

        val codeGenerationExceptions = controller.getCodeGenerationExceptions
        if (codeGenerationExceptions.nonEmpty) {
            logger.error("Code generation errors:")
            codeGenerationExceptions.foreach( (f: CodeGenerationException) => logger.error(f.getMessage))
            errorQuit("Cannot continue")
        }

        val startOutputTime = System.currentTimeMillis()
        controller.output(model, outputFile, binaryFile, listingFile)
        val endOutputTime = System.currentTimeMillis()
        logger.debug(s"Output complete in ${endOutputTime - startOutputTime} ms")
    }

    // -----------------------------------------------------------------------------------------------------------------

    def errorQuit(str: String): Unit = {
        logger.error(str)
        quit()
    }

    def usage() {
        version()
        logger.info("tmasm [options] file.asm")
        logger.info("Assembler options:")
        logger.info("--help, -?                 - just display this help text")
        logger.info("--version                  - just display the version of tmasm")
        logger.info("-o|--output output.o       - create an ELF output file")
        logger.info("-b|--binary output         - create a binary output file")
        logger.info("-l|--listing output.lst    - create a listing file")
        logger.info("-I|--includepath directory - add directory to include search paths")
        logger.info("-x|--caseSensitive         - treat variable, constant and label names as case-sensitive")
        logger.info("                             (default is insensitive: they are converted to upper case)")
        logger.info("Diagnostics:")
        logger.info("-e|--expansion             - enable macro expansion diagnostics")
        logger.info("-p|--parser                - enable parser diagnostics")
        logger.info("-P|--showParserOutput      - show parser text input and AST output")
        logger.info("-c|--codegen               - enable code generation diagnostics")
        logger.info("Logging output control options:")
        logger.info("--debug                    - set the log level to debug (default is info)")
        logger.info("--warn                     - set the log level to warning")
        logger.info("--level                    - show log levels of each log line output")
        logger.info("--classes                  - show class names in each log line output")
        logger.info("--threads                  - show thread names in each log line output")
        logger.info("--times                    - show timing data in each log line output")
    }

    def version() {
        logger.info(s"${AssemblerMain.appName} ${Version.getPropertiesVersion()}")
    }


    def quit() {
        System.exit(1)
    }

    def exit() {
        System.exit(0)
    }
}

object AssemblerMain {
    private val appName = "Transputer Macro Assembler"

    /**
      * @param args the command line arguments.
      */
    def main(args: Array[String]) {
        val finalArgList = LogbackLogging.setupLoggingFromArgs(args.toList)
        new AssemblerMain(finalArgList).start()
    }
}


