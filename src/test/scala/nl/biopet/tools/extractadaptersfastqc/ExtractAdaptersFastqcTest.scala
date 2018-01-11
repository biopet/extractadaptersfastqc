package nl.biopet.tools.extractadaptersfastqc

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
}
