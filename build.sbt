lazy val root = (project in file(".")).settings(
  name := "asm16",
  version := "0.1.0",
  scalaVersion := "2.12.0",
  organization := "ditrapani.info"
)

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

assemblyJarName in assembly := s"ljd-${name.value}-${version.value}.jar"
