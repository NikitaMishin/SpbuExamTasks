name := "BurovaExam"

version := "0.1"
libraryDependencies += "org.scalanlp" % "breeze_2.11" % "0.12"
libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test")
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
scalaVersion := "2.11.6"

parallelExecution in Test := true
