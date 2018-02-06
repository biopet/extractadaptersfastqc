organization := "com.github.biopet"
organizationName := "Sequencing Analysis Support Core - Leiden University Medical Center"

startYear := Some(2018)

name := "ExtractAdaptersFastqc"
biopetUrlName := "extractadaptersfastqc"

biopetIsTool := true

mainClass in assembly := Some(s"nl.biopet.tools.${name.value.toLowerCase()}.${name.value}")

developers += Developer(id="ffinfo", name="Peter van 't Hof", email="pjrvanthof@gmail.com", url=url("https://github.com/ffinfo"))

scalaVersion := "2.11.11"

libraryDependencies += "com.github.biopet" %% "ToolUtils" % "0.3-SNAPSHOT" changing()
libraryDependencies += "com.github.biopet" %% "NgsUtils" % "0.3-SNAPSHOT" changing()
libraryDependencies += "com.github.biopet" %% "ToolTestUtils" % "0.2-SNAPSHOT" changing()
