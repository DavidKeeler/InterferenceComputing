lazy val root = (project in file(".")).
  settings(
    name := "InferenceComputing",
    version := "1.0",
    scalaVersion := "2.11.7"
  )

libraryDependencies ++= Seq(
  "com.github.rodneykinney" %%  "quisp" % "0.6.0"
//  "jfree" %% "jfreechart" % "1.0.19"
)

// https://mvnrepository.com/artifact/org.jfree/jfreechart
libraryDependencies += "org.jfree" % "jfreechart" % "1.0.17"

// https://mvnrepository.com/artifact/org.apache.commons/commons-io
//libraryDependencies += "org.apache.commons" % "commons" % "1.3.2"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5"
