package info.ditrapani.tilecreator

import scala.util.{Try, Success, Failure}

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    val file_name = "src/test/resources/built-in.tiles"

    Try(scala.io.Source.fromFile(file_name).mkString) match {
      case Failure(exception) => println(exception)
      case Success(str) => parseStr(str)
    }
  }

  def parseStr(str: String): Unit = {
    import fastparse.all._

    val parseFile = P("")
    val parseA = P("a")
    println(parseA.parse("a"))
    println(parseA.parse("b"))
  }
}

case class Adder(val base: Int) {
  def add(x: Int): Adder = Adder(base + x)
}
