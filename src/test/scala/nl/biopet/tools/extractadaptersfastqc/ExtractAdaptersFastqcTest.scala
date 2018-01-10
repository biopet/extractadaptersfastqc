package nl.biopet.tools.extractadaptersfastqc

import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

class ExtractAdaptersFastqcTest extends ToolTest[Args] {
  def toolCommand: ToolTemplate.type = ToolTemplate
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      ToolTemplate.main(Array())
    }
  }
}
