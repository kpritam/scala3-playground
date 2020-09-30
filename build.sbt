val dottyVersion = "0.28.0-bin-20200928-09eaed7-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    scalaVersion := dottyLatestNightlyBuild.get,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
