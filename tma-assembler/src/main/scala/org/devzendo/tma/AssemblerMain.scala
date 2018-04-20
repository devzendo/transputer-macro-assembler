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

import java.io.File
import java.util

import org.devzendo.commoncode.resource.ResourceLoader

import scala.io.Source

class AssemblerMain(val argList: List[String]) {
    val logger = org.log4s.getLogger

    val assemblerProperties = loadAssemblerProperties()

    var argIndex = 0
    var asmFile: Option[File] = None
    var outputFile: Option[File] = None
    var binaryFile: Option[File] = None
    var listingFile: Option[File] = None

    def existingFile(fileType: String, f: String): Option[File] = {
        val file = new File(f)
        if (file.exists()) {
            Some(file)
        } else {
            logger.error("The " + fileType + " file '" + f + "' does not exist")
            quit()
            None // won't get here
        }
    }

    while (argIndex < argList.length)
    {
        val f = argList(argIndex)

        def expectFileName(): Option[File] = {
            if (argIndex == argList.length - 1) {
                logger.error(f + " requires a file as its argument")
                quit()
                None // won't get here
            }
            val ret = Some(new File(argList(argIndex + 1)))
            argIndex += 1
            ret
        }

        logger.debug("ARG: [" + f + "]")
        f match {
            case "--help" => { usage(); exit() }
            case "-?" => { usage(); exit() }
            case "--version"  => { version(); exit() }

            case "-o" | "--output" => {
                outputFile = expectFileName()
            }

            case "-b" | "--binary" => {
                binaryFile = expectFileName()
            }

            case "-l" | "--listing" => {
                listingFile = expectFileName()
            }

            case _ => {
                if (f.startsWith("-")) {
                    logger.error("Unknown command line option: '" + f + "'")
                    logger.error("")
                    usage()
                    quit()
                }
                asmFile = existingFile("assembly", f)
            }
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
        val parser = new AssemblyParser()
        Source.fromFile(asmFile.get).getLines().zipWithIndex.foreach(parser.parse)
        val model: AssemblyModel = parser.createModel

        outputFile.foreach(new ELFWriter(_).encode(model))
        binaryFile.foreach(new BinaryWriter(_).encode(model))
        listingFile.foreach(new ListingWriter(_).encode(model))
    }

    // -----------------------------------------------------------------------------------------------------------------

    def errorQuit(str: String) = {
        logger.error(str)
        quit()
    }

    def usage() {
        version()
        logger.info("tmasm [options] file.asm")
        logger.info("Assembler options:")
        logger.info("--help, -?               - just display this help text")
        logger.info("--version                - just display the version of tmasm")
        logger.info("-o|--output output.o     - create an ELF output file")
        logger.info("-b|--binary output       - create a binary output file")
        logger.info("-l|--listing output.lst  - create a listing file")
        logger.info("Logging output control options:")
        logger.info("--debug                  - set the log level to debug (default is info)")
        logger.info("--warn                   - set the log level to warning")
        logger.info("--level                  - show log levels of each log line output")
        logger.info("--classes                - show class names in each log line output")
        logger.info("--threads                - show thread names in each log line output")
        logger.info("--times                  - show timing data in each log line output")
    }

    def loadAssemblerProperties(): util.Properties = {
        val propertiesResourceName = "assembler.properties"
        val propertiesResource = ResourceLoader.readPropertiesResource(propertiesResourceName)
        if (propertiesResource == null) {
            logger.error("Could not load " + propertiesResourceName)
            exit()
        }
        propertiesResource
    }

    def getPropertiesVersion() = {
        assemblerProperties.getProperty("version")
    }

    def version() {
        logger.info(AssemblerMain.appName + " " + getPropertiesVersion())
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


