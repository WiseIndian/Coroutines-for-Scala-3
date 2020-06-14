val dottyVersion = "0.22.0-RC1"



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
      "com.novocode" % "junit-interface" % "0.11" % "test",
    ),


    javaOptions ++= Seq("-Xms1G", "-Xmx1G"),



    parallelExecution in Test := false

  ) 
