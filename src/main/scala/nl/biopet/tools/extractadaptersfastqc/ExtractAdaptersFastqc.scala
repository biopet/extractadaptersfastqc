/*
 * Copyright (c) 2018 Biopet
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
import nl.biopet.utils.io.writeLinesToFile

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

    require(
      adapterSet
        .getOrElse(Set())
        .nonEmpty || contaminantSet.getOrElse(Set()).nonEmpty,
      "No known adapters or contaminations found, please supply the fastqc files"
    )

    val adapters =
      foundAdapters(fastqcData, cmdArgs.adapterCutoff, adapterSet.toSet.flatten)
    val contams =
      foundOverrepresented(fastqcData, contaminantSet.toSet.flatten)

    writeOutput(adapters, cmdArgs.adapterOutputFile, cmdArgs.outputAsFasta)
    writeOutput(contams, cmdArgs.contamsOutputFile, cmdArgs.outputAsFasta)

    logger.info("Done")
  }

  /** This will write the output to a file or to stdout when none file supplied */
  def writeOutput(sequences: Set[AdapterSequence],
                  outputFile: Option[File],
                  outputAsFasta: Boolean = false): Unit = {
    val content =
      if (outputAsFasta) sequences.map(_.asFastaLine).mkString("\n")
      else sequences.map(_.seq).mkString("\n")
    outputFile match {
      case Some(file) => writeLinesToFile(file, content :: Nil)
      case _          => println(content)
    }

  }

  case class FastQCModule(name: String, status: String, lines: Seq[String])
  case class AdapterSequence(name: String, seq: String) {
    def asFastaLine: String = s">$name\n$seq"
  }

  /**
    * FastQC QC modules.
    *
    * @param dataFile File to read
    * @return Mapping of FastQC module names and its contents as array of strings (one item per line)
    * @throws FileNotFoundException if the FastQC data file can not be found.
    * @throws IllegalStateException if the module lines have no content or mapping is empty.
    */
  def qcModules(dataFile: File): Map[String, FastQCModule] = {
    Source
      .fromFile(dataFile)
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
          modName -> FastQCModule(modName,
                                  modStatus,
                                  otherLines.split("\n").toSeq)
      }
      .toMap
  }

  /** This will read known sequences */
  def getFastqcSeqs(file: File): Set[AdapterSequence] = {
    (for {
      line <- Source.fromFile(file).getLines()
      if !line.startsWith("#")
      values = line.split("\t+")
      if values.size >= 2
    } yield AdapterSequence(values(0), values(1))).toSet
  }

  /**
    * This method will find the adapters sequences from the fastqc data
    * @param fastqcModules Content of the fastqc data file
    * @param adapterCutoff Cutoff for adapters
    * @param adapterSet Known sequences
    * @return Found sequences
    */
  def foundAdapters(fastqcModules: Map[String, FastQCModule],
                    adapterCutoff: Double,
                    adapterSet: Set[AdapterSequence]): Set[AdapterSequence] = {
    fastqcModules
      .get("Adapter Content")
      .map { x =>
        val header =
          x.lines.headOption.getOrElse("").split("\t").tail.zipWithIndex
        val lines = x.lines.tail.map(_.split("\t").tail)
        val found = header
          .filter {
            case (_, idx) => lines.exists(x => x(idx).toFloat > adapterCutoff)
          }
          .map { case (name, _) => name }
        adapterSet.filter(x => found.contains(x.name))
      }
      .getOrElse(Set())
  }

  /**
    * This method will find the overrepresented sequences from the fastqc data
    * @param fastqcModules Content of the fastqc data file
    * @param contaminantSet Known sequences
    * @return Found sequences
    */
  def foundOverrepresented(
      fastqcModules: Map[String, FastQCModule],
      contaminantSet: Set[AdapterSequence]): Set[AdapterSequence] = {
    val foundAdapterNames: Seq[String] =
      fastqcModules.get("Overrepresented sequences") match {
        case None => Seq.empty[String]
        case Some(qcModule) =>
          for (line <- qcModule.lines
               if !(line.startsWith("#") || line.startsWith(">"));
               values = line.split("\t") if values.size >= 4) yield values(3)
      }

    // select full sequences from known adapters and contaminants
    // based on overrepresented sequences results
    contaminantSet
      .filter(x => foundAdapterNames.exists(_.startsWith(x.name)))
  }

  def descriptionText: String =
    s"""
      |$toolName reads which adapter sequences where found from a FastQC raw report.
      |These sequences can be used as input for a QC tool such as cutadapt.
      |The sequences can be output in plain text format with a
      |newline character as a separator between the sequences.
      |Alternatively the sequences can be output in FASTA format.
    """.stripMargin
  def manualText: String =
    s"""
      |The tool wil only find sequences that are known to fastqc. This is by default defined in this files:
      |- <fastqc_dir>/Configuration/adapter_list.txt
      |- <fastqc_dir>/Configuration/contaminant_list.txt
      |
      |These files are required for this tool to find the correct adapters.
      |
      |The adapter list is only available to fastqc 0.11+
    """.stripMargin

  def exampleText: String =
    s"""
      |A default run would look like this, output will go to stdout:
      |${example("-i",
                 "<fastqc_data_file>",
                 "--knownContamFile",
                 "<contems_file>",
                 "--knownAdapterFile",
                 "<adapter_file>")}
      |
      |To select output files:
      |${example(
         "-i",
         "<fastqc_data_file>",
         "--knownContamFile",
         "<contems_file>",
         "--knownAdapterFile",
         "<adapter_file>",
         "--adapterOutputFile",
         "<output_file>",
         "--contamsOutputFile",
         "<output_file>"
       )}
    """.stripMargin
}
