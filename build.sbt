lazy val root = (project in file(".")).
  settings(
    name := "Tile Creator",
    version := "0.1",
    scalaVersion := "2.11.8"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
