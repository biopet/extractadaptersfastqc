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

import java.io.File

import nl.biopet.utils.tool.{AbstractOptParser, ToolCommand}

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {
  opt[File]('i', "inputFile")
    .required()
    .action((x, c) => c.copy(dataFile = x))
    .text("Fastqc data file")
  opt[File]("adapterOutputFile")
    .action((x, c) => c.copy(adapterOutputFile = Some(x)))
    .text("Output file for adapters, is not supplied output will go to stdout")
  opt[File]("contamsOutputFile")
    .action((x, c) => c.copy(contamsOutputFile = Some(x)))
    .text("Output file for adapters, is not supplied output will go to stdout")
  opt[Unit]("skipContams")
    .action((_, c) => c.copy(contaminated = false))
    .text("If this is set only the adapters block is used, other wise contamination is also used")
  opt[File]("contamFile")
    .action((x, c) => c.copy(contamFile = Some(x)))
    .text("This is the file that does contain the known contamination from fastqc")
  opt[File]("adapterFile")
    .action((x, c) => c.copy(adapterFile = Some(x)))
    .text("This is the file that does contain the known adapters from fastqc")
  opt[Double]("adapterCutoff")
    .action((x, c) => c.copy(adapterCutoff = x))
    .text(s"The fraction of the adapters in a read should be above this fraction, default is ${Args().adapterCutoff}")
  opt[Unit]("outputAsFasta")
    .action((_, c) => c.copy(outputAsFasta = true))
    .text(s"Output in fasta format, default only sequences")
}
