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

package org.devzendo.tma.util

import java.io.File

import org.junit.rules.TemporaryFolder
import org.junit.{After, Before}
import org.log4s.Logger

trait TempFolder {
    val logger: Logger = org.log4s.getLogger

    private[this] var tempDir: TemporaryFolder = null
    var temporaryDirectory: File = null

    @Before
    def createTempDirectoryBefore() {
        tempDir = new TemporaryFolder()
        tempDir.create()
        temporaryDirectory = tempDir.getRoot
        logger.info("temp directory is " + temporaryDirectory.getAbsolutePath)
    }

    @After
    def deleteTempDirectoryAfter() {
        logger.info("tidying up temp dir " + temporaryDirectory.getAbsolutePath)
        tempDir.delete()
    }

}