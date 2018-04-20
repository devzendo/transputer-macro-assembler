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

import java.time.{Instant, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.core.encoder.LayoutWrappingEncoder
import ch.qos.logback.core.{ConsoleAppender, LayoutBase}
import org.slf4j.Logger

import scala.collection.mutable

object LogbackLogging {
    private val lineSep: String = System.getProperty("line.separator")

    /**
      * Configure console output for Scala programs using log4s as a Scala wrapper around slf4j, and logback-classic
      * as the actual framework.
      * Sets up log4j given command line arguments, called only once at the start
      * of main, with the command line args. Changes to the layout (for example)
      * can be made after this call.
      *
      * @param origArgs the command line arguments
      * @return those arguments with the logging arguments removed
      */
    def setupLoggingFromArgs(origArgs: List[String]): List[String] = {
        val out = new mutable.ListBuffer[String]
        var bLevel = false
        var mDebug = false
        var mWarn = false
        var bClasses = false
        var bThreads = false
        var bTimes = false
        for (arg <- origArgs) {
            arg match {
                case "--debugall" => {
                    bLevel = true
                    mDebug = true
                    bClasses = true
                    bThreads = true
                    bTimes = true
                }
                case "--level" =>
                    bLevel = true
                case "--debug" =>
                    mDebug = true
                case "--warn" =>
                    mWarn = true
                case "--classes" =>
                    bClasses = true
                case "--threads" =>
                    bThreads = true
                case "--times" =>
                    bTimes = true
                case _ =>
                    out += arg
            }
        }

        val lc = new LoggerContext()

        val ca = new ConsoleAppender[ILoggingEvent]
        ca.setContext(lc)
        ca.setName("console")
        val encoder = new LayoutWrappingEncoder[ILoggingEvent]
        encoder.setContext(lc)

        val layout = createLayout(bLevel, bClasses, bThreads, bTimes)

        layout.setContext(lc)
        layout.start()
        encoder.setLayout(layout)

        ca.setEncoder(encoder)
        ca.start()

        val rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME)
        rootLogger.addAppender(ca)

        rootLogger.setLevel(
            if (mDebug) Level.DEBUG
            else if (mWarn) Level.WARN
            else Level.INFO)

        out.toList
    }

    private def createLayout(bLevel: Boolean, bClasses: Boolean, bThreads: Boolean, bTimes: Boolean) = {
        val sb = new StringBuilder
        if (bLevel) sb.append("%-5p ")
        if (bTimes) sb.append("%d{ISO8601} ")
        if (bClasses) sb.append("%c{1} ")
        if (bThreads) sb.append("[%t] ")
        sb.append("%m\n")
        new LayoutBase[ILoggingEvent] {
            override def doLayout(event: ILoggingEvent): String = {

                val sbuf = new StringBuffer(128)
                if (bLevel) {
                    sbuf.append(event.getLevel)
                    sbuf.append(" ")
                }
                if (bTimes) {
                    val timeInLong = event.getTimeStamp - event.getLoggerContextVO.getBirthTime
                    val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(timeInLong), ZoneId.systemDefault)
                    sbuf.append(zdt.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
                    sbuf.append(" ")
                }
                if (bClasses) {
                    sbuf.append(event.getLoggerName)
                    sbuf.append(" ")
                }
                if (bThreads) {
                    sbuf.append("[")
                    sbuf.append(event.getThreadName)
                    sbuf.append("] ")
                }
                sbuf.append(event.getFormattedMessage)
                sbuf.append(lineSep)
                sbuf.toString
            }
        }
    }
}