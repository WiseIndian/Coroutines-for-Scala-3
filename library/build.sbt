val dottyVersion = "0.22.0-RC1"

//for scalameter benchmarking
lazy val Benchmark = config("bench") extend Test

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    resolvers += "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/releases",

    libraryDependencies += "com.storm-enroute" % "scalameter_2.13" % "0.19",


    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),


    parallelExecution in Test := false

  ) configs(
    Benchmark
  ) settings(
    inConfig(Benchmark)(Defaults.testSettings): _*
  )
