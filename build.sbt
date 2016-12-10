lazy val root = (project in file(".")).settings(
  name := "asm16",
  version := "0.1.0",
  scalaVersion := "2.12.1",
  organization := "ditrapani.info"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-Xlint",
  "-Ywarn-unused-import",
  "-Ywarn-unused",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen"
)

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

wartremoverWarnings ++= Warts.allBut(
  Wart.Equals,
  Wart.MutableDataStructures,
  Wart.NoNeedForMonad,
  Wart.NonUnitStatements,
  Wart.Nothing,
  Wart.Overloading
)

assemblyOutputPath in assembly := baseDirectory.value / s"ljd-${name.value}-${version.value}.jar"
