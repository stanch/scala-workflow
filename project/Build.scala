import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalacOptions ++= Seq("-feature"),
    scalaVersion := "2.10.3-RC1",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      compilerPlugin("org.scala-lang.plugins" % "macro-paradise_2.10.3-RC1" % "2.0.0-SNAPSHOT"),
      "org.scala-lang" % "scala-reflect" % "2.10.3-RC1"
    )
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("core"),
    settings = buildSettings ++ Seq(
      initialCommands in console := "import workflow._; import Shortcuts._",
      libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
    )
  )
}
