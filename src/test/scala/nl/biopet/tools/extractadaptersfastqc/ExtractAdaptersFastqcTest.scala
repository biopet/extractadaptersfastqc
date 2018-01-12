package nl.biopet.tools.extractadaptersfastqc

import java.io.File

import nl.biopet.utils.IoUtils
import nl.biopet.utils.test.tools.ToolTest
import nl.biopet.utils.tool.ToolCommand
import org.testng.annotations.Test

class ExtractAdaptersFastqcTest extends ToolTest[Args] {
  def toolCommand: ToolCommand[Args] = ExtractAdaptersFastqc
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      ExtractAdaptersFastqc.main(Array())
    }
  }

  @Test
  def testDefault(): Unit = {
    val adapterOutput = File.createTempFile("test.", ".txt")
    adapterOutput.deleteOnExit()
    val contamOutput = File.createTempFile("test.", ".txt")
    contamOutput.deleteOnExit()
    ExtractAdaptersFastqc.main(Array(
      "-i", resourcePath("/fastqc_data.txt"),
      "--knownContamFile", resourcePath("/contaminant_list.txt"),
      "--knownAdapterFile", resourcePath("/adapter_list.txt"),
      "--adapterOutputFile", adapterOutput.getAbsolutePath,
      "--contamsOutputFile", contamOutput.getAbsolutePath
    ))

    IoUtils.getLinesFromFile(adapterOutput) shouldBe List("AGATCGGAAGAG")
    IoUtils.getLinesFromFile(contamOutput) shouldBe List(
      "GATCGGAAGAGCACACGTCTGAACTCCAGTCACGTCCGCATCTCGTATGCCGTCTTCTGCTTG",
      "GATCGGAAGAGCACACGTCTGAACTCCAGTCACATCACGATCTCGTATGCCGTCTTCTGCTTG")
  }
}
