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

package org.devzendo.tma

import java.io.{File, FileNotFoundException, IOException}

import org.log4s.Logger

import scala.collection.mutable
import scala.io.Source

/**
 * Given an initial file, read it line-by-line, producing an Iterator of source file/line/text for processing by a parser.
 * If the parser determines that a line contains an include directive, the filename in the directive can be pushed
 * onto the stack of files that this reader maintains. Subsequent items in the iterator will come from the include file
 * until its EOF is reached, upon which it is popped and reading continues from the file that included it.
 *
 * TODO accept multiple include paths, under which each pushed include file will be searched for.
 */

/**
 * SourceLocations give file names and locations.
 */
case class SourceLocation(fileName: String, lineNumber: Int)

/**
 * SourceItems are returned in the Iterator from the SourceIncludingReader's openSourceIterator method.
 * Line numbers start from 1, the first line of the file. The current file name and line number are given by
 * currentSourceLocation, and if there's any inclusion being processed (up to an arbitrary depth), nestedLocations
 * will give the stack of of files, e.g. if file1 includes file2 which includes file3, then when file3 is being read,
 * the list of nested file names will be [file1, file2, file3]. The locations of each of the nested files are given as
 * part of the SourceLocation.
 */
case class SourceItem(nestedLocations: List[SourceLocation], currentSourceLocation: SourceLocation, line: String) {
    def currentSourceLocationPath: String = {
        nestedLocations.map((sc: SourceLocation) => sc.fileName + ":" + sc.lineNumber).mkString("/")
    }
}

/**
 * The Includer trait is used by the parser when it parses an INCLUDE directive.
 */
trait Includer {
    def pushIncludeFile(includeFile: File): Unit
}

class SourceIncludingReader extends Includer {
    private val logger: Logger = org.log4s.getLogger

    case class SourceContext(file: File, var lineNumber: Int, iterator: Iterator[String])
    val contexts = new mutable.ArrayBuffer[SourceContext]

    private val includePaths = new mutable.ArrayBuffer[File]

    /**
     * Add an include path to the reader. These paths will be searched in order when loading an include file.
     */
    def addIncludePath(path: File): Unit = {
        if (path.exists) {
            if (!path.isDirectory) {
                throw new IOException("Include path '" + path.getName + "' is not a directory")
            }
        } else {
            throw new FileNotFoundException("Include path '" + path.getName + "' does not exist")
        }
        includePaths += path
    }

    /**
     * Open the first input file, and return a Iterator of SourceItems containing its data.
     * @param firstInputFile The input file name. The file may contain include directives, which when processed by a
     *                       parser, cause calls to pushIncludeFile.
     * @return The Iterator of SourceItems.
     */
    def openSourceIterator(firstInputFile: File): Iterator[SourceItem] = {
        val firstInputFileName = firstInputFile.getName
        logger.debug("Starting source iterator with " + firstInputFileName)
        val lineIterator = Source.fromFile(firstInputFile).getLines()
        var lineNumber = 0
        contexts += SourceContext(firstInputFile, 0, lineIterator)
        new Iterator[SourceItem] {
            override def hasNext: Boolean = {
                while(true) {
                    if (contexts.isEmpty) {
                        logger.debug("End of input")
                        return false
                    }
                    val lastContext = contexts.last
                    val hasNext = lastContext.iterator.hasNext
                    if (hasNext) {
                        return true
                    } else {
                        logger.debug("Popping include file " + lastContext.file.getName)
                        contexts.remove(contexts.size - 1)
                    }
                }
                false // will never get here, but compiler can't see that
            }
            override def next(): SourceItem = {
                val lastContext = contexts.last
                lastContext.lineNumber = lastContext.lineNumber + 1
                val nestedLocations = contexts.map( (sc: SourceContext) => { SourceLocation(sc.file.getName, sc.lineNumber) } ).toList
                SourceItem(nestedLocations, SourceLocation(lastContext.file.getName, lastContext.lineNumber), lastContext.iterator.next())
            }
        }
    }

    def pushIncludeFile(includeFile: File): Unit = {
        logger.debug("Pushing include file " + includeFile.getName)
        // If it's an absolute path, don't bother searching through include paths...
        if (includeFile.isAbsolute) {
            if (includeFile.exists()) {
                logger.debug("Found absolute-pathed include file " + includeFile.getName + " at " + includeFile.getAbsolutePath)
                val lineIterator = Source.fromFile(includeFile).getLines()
                contexts += SourceContext(includeFile, 0, lineIterator)
                return
            } else {
                throw new FileNotFoundException("Not found include file '" + includeFile.getName + "' at " + includeFile.getAbsolutePath)
            }
        }

        // Search through all includePaths to see if includeFile exists
        val here = new File(".")
        val tail = includePaths.toList
        val allIncludePaths: List[File] = here :: tail
        for (includePath <- allIncludePaths) {
            val pathedIncludeFile = new File(includePath, includeFile.toString)
            if (pathedIncludeFile.exists()) {
                logger.debug("Found relative-pathed include file " + includeFile.getName + " at " + pathedIncludeFile.getAbsolutePath)
                val lineIterator = Source.fromFile(pathedIncludeFile).getLines()
                contexts += SourceContext(includeFile, 0, lineIterator)
                return
            } else {
                logger.debug("Not found include file " + includeFile.getName + " at " + pathedIncludeFile.getAbsolutePath)
            }
        }
        throw new FileNotFoundException("Include file '" + includeFile.getName + "' not found in current directory or any include path")
    }
}
