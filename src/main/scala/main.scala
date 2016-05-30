package info.ditrapani.tilecreator

object Main {
  def main(args: Array[String]): Unit = println("Hello world!")
}

case class Adder(val base: Int) {
  def add(x: Int): Adder = Adder(base + x)
}
