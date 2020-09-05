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

import java.io.File

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
 * SourceItems are returned in the Iterator from the SourceIncludingReader's openSourceIterator method.
 * Line numbers start from 1, the first line of the file. The current file name is given by fileName, and if there's any
 * inclusion being processed (up to an arbitrary depth), nestedFileNames will give the full path of files, e.g. if file1
 * includes file2 which includes file3, then when file3 is being read, the list of nested file names will be [file1,
 * file2, file3].
 */
case class SourceItem(nestedFileNames: List[String], fileName: String, lineNumber: Int, line: String)

class SourceIncludingReader {

    /**
     * Open the first input file, and return a Iterator of SourceItems containing its data.
     * @param firstInputFile The input file name. The file may contain include directives, which when processed by a
     *                       parser, cause calls to pushIncludeFile.
     * @return The Iterator of SourceItems.
     */
    def openSourceIterator(firstInputFile: File): Iterator[SourceItem] = {
        val lineIterator = Source.fromFile(firstInputFile).getLines()
        var lineNumber = 0
        val firstInputFileName = firstInputFile.getName
        new Iterator[SourceItem] {
            override def hasNext: Boolean = lineIterator.hasNext
            override def next(): SourceItem = {
                lineNumber = lineNumber + 1
                SourceItem(null, firstInputFileName, lineNumber, lineIterator.next())
            }
        }
//        //lineIterator.zipWithIndex.foreach((p: (String, Int)) => parseTextLine(p._2 + 1, p._1))
    }

    def pushIncludeFile(includeFile: File): Unit = {

    }

}
