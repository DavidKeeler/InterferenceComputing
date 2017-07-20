
name := "InferenceComputing"

organization := "org.none"

version := "0.1.0"

// set the Scala version used for the project
scalaVersion := "2.11.6"

// https://mvnrepository.com/artifact/com.github.wookietreiber/scala-chart_2.11
libraryDependencies += "com.github.wookietreiber" % "scala-chart_2.11" % "0.5.1"

// https://mvnrepository.com/artifact/com.itextpdf/itextpdf
libraryDependencies += "com.itextpdf" % "itextpdf" % "5.5.6"

// https://mvnrepository.com/artifact/org.scalatest/scalatest_2.11
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5"

// https://mvnrepository.com/artifact/org.scalanlp/breeze_2.11
libraryDependencies += "org.scalanlp" % "breeze_2.11" % "0.13.1"



resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)


