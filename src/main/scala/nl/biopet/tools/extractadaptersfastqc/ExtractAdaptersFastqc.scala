/*
 * Copyright (c) 2017 Sequencing Analysis Support Core - Leiden University Medical Center
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.tools.extractadaptersfastqc

import java.io.{File, FileNotFoundException}

import nl.biopet.utils.tool.ToolCommand

import scala.io.Source

object ExtractAdaptersFastqc extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    val fastqcData = qcModules(cmdArgs.dataFile)
    val adapterSet = cmdArgs.adapterFile.map(getFastqcSeqs)
    val contaminantSet = cmdArgs.contamFile.map(getFastqcSeqs)

    val adapters = foundAdapters(fastqcData, cmdArgs.adapterCutoff, adapterSet.toSet.flatten)

    //TODO: Get contams
    //TODO: write output

    logger.info("Done")
  }

  case class FastQCModule(name: String, status: String, lines: Seq[String])
  case class AdapterSequence(name: String, seq: String)

  /**
    * FastQC QC modules.
    *
    * @param dataFile File to read
    * @return Mapping of FastQC module names and its contents as array of strings (one item per line)
    * @throws FileNotFoundException if the FastQC data file can not be found.
    * @throws IllegalStateException if the module lines have no content or mapping is empty.
    */
  def qcModules(dataFile: File): Map[String, FastQCModule] = {
    Source.fromFile(dataFile)
      // drop all the characters before the first module delimiter (i.e. '>>')
      .dropWhile(_ != '>')
      // pull everything into a string
      .mkString
      // split into modules
      .split(">>END_MODULE\n")
      // make map of module name -> module lines
      .map {
      case (modString) =>
        // module name is in the first line, without '>>' and before the tab character
        val Array(firstLine, otherLines) = modString
          // drop all '>>' character (start of module)
          .dropWhile(_ == '>')
          // split first line and others
          .split("\n", 2)
          // and slice them
          .slice(0, 2)
        // extract module name and module status
        val Array(modName, modStatus) = firstLine
          .split("\t", 2)
          .slice(0, 2)
        modName -> FastQCModule(modName, modStatus, otherLines.split("\n").toSeq)
    }
      .toMap
  }

  def getFastqcSeqs(file: File): Set[AdapterSequence] = {
    (for {
      line <- Source.fromFile(file).getLines()
      if !line.startsWith("#")
      values = line.split("\t+")
      if values.size >= 2
    } yield AdapterSequence(values(0), values(1))).toSet
  }

  def foundAdapters(fastqcModules: Map[String, FastQCModule],
                    adapterCutoff: Double,
                    adapterSet: Set[AdapterSequence]): Option[Set[AdapterSequence]] = {
    fastqcModules.get("Adapter Content").map { x =>
      val header = x.lines.head.split("\t").tail.zipWithIndex
      val lines = x.lines.tail.map(_.split("\t").tail)
      val found = header
        .filter(h => lines.exists(x => x(h._2).toFloat > adapterCutoff))
        .map(_._1)
      adapterSet.filter(x => found.contains(x.name))
    }
  }

  def descriptionText: String =
    """
      |
    """.stripMargin
  def manualText: String =
    s"""
      |
    """.stripMargin

  def exampleText: String =
    """
      |
    """.stripMargin
}
