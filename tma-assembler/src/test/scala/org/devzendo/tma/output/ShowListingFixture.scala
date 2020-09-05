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

package org.devzendo.tma.output

import java.io.File

import org.devzendo.tma.codegen.AssemblyModel
import org.log4s.Logger

import scala.io.Source

trait ShowListingFixture {
    val logger: Logger

    def showListing(model: AssemblyModel): Unit = {
        val listingFile: File = File.createTempFile("out.", ".lst", new File(System.getProperty("java.io.tmpdir")))
        try {
            val writer: ListingWriter = new ListingWriter(listingFile)
            writer.encode(model)
            val lines: Array[String] = Source.fromFile(listingFile).getLines().toArray
            logger.info("")
            logger.info("-" * 80)
            lines.foreach(line => logger.info(line))
            logger.info("-" * 80)
        } finally {
            listingFile.delete()
        }
    }
}
