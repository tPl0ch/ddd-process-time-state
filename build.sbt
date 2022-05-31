ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.1.2"

lazy val V = new {
  val cats = "2.7.0"
}

lazy val dependencies = new {
  val cats = "org.typelevel" %% "cats-core" % V.cats
}

lazy val root = (project in file("."))
  .settings(
    name             := "process-time-state",
    idePackagePrefix := Some("org.tp.process_time_state"),
    libraryDependencies ++= Seq(
      dependencies.cats
    )
  )
