ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion      := "3.1.2"
ThisBuild / semanticdbEnabled := true

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-deprecation",
  "-unchecked",
  "-source:future",
  "-explain-types",
)

lazy val V = new {
  val cats       = "2.7.0"
  val catsEffect = "3.3.12"
}

lazy val dependencies = new {
  val cats       = "org.typelevel" %% "cats-core"   % V.cats
  val catsEffect = "org.typelevel" %% "cats-effect" % V.catsEffect
}

lazy val root = (project in file("."))
  .settings(
    name             := "process-time-state",
    idePackagePrefix := Some("org.tp.process_time_state"),
    libraryDependencies ++= Seq(
      dependencies.cats,
      dependencies.catsEffect,
    ),
  )
