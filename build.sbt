lazy val root = (project in file(".")).settings(
  name := "tile-creator",
  version := "0.1.0",
  scalaVersion := "2.11.8",
  organization := "ditrapani.info"
)

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

assemblyJarName in assembly := s"ljd-${name.value}-${version.value}.jar"
