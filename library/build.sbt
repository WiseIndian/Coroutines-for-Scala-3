val dottyVersion = "0.22.0-RC1"

//for scalameter benchmarking
lazy val Benchmark = config("bench") extend Test


//for benchmarking c.f. project/plugins.sbt
enablePlugins(JmhPlugin)

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-coroutines",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    
    resolvers += "Sonatype OSS Snapshots" at
    "https://oss.sonatype.org/content/repositories/releases",
    
    libraryDependencies ++= Seq(
      "com.storm-enroute" % "scalameter_2.13" % "0.19",
      "com.novocode" % "junit-interface" % "0.11" % "test",
    ),



    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),


    parallelExecution in Test := false

  ) configs(
    Benchmark
  ) settings(
    inConfig(Benchmark)(Defaults.testSettings): _*
  )
