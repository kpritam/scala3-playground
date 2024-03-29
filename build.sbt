val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "scala3-playground",
    version      := "0.1.0",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-explain",
//      "-indent",
      "-new-syntax",
      "-rewrite"
//      "-language:strictEquality"
    )
  )
