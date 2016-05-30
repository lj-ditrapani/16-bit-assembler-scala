package info.ditrapani.tilecreator


object Main {
  def main(args: Array[String]): Unit = {
    import scala.util.{Try, Success, Failure}

    val file_name = "src/test/resources/built-in.tiles"

    Try(scala.io.Source.fromFile(file_name).mkString) match {
      case Failure(exception) => println(exception)
      case Success(str) => parseStr(str)
    }
  }

  def parseStr(str: String): Unit = {
    import fastparse.all._

    val large_tile_header = P("Large Tiles\n")
    val large_tile_ruler = P("  0 1 2 3 4 5 6 7 8 9 A B C D E F\n")
    val pixel0 = P("[]").map((x) => 0)
    val pixel1 = P("()").map((x) => 1)
    val pixel2 = P("{}").map((x) => 2)
    val pixel3 = P("<>").map((x) => 3)
    val pixel = P(pixel0 | pixel1 | pixel2 | pixel3)
    val large_tile_row = P("  " ~/ pixel.rep(min = 16, max = 16))
    val tile_num = P(CharIn("0123456789ABCDEF").rep(min = 2, max = 2) ~/ "\n")
    val large_tile =
      P(tile_num ~/ large_tile_row.rep(min = 16, max = 16, sep = "\n") ~/ "\n")
    val large_tile_set = P(
      large_tile_header ~/
      large_tile_ruler ~/
      large_tile.rep(min = 64, max = 64)
    )
    val small_tile_header = P("Small Tiles\n")
    val small_tile_ruler = P("  0 1 2 3 4 5 6 7\n")
    val small_tile_row = P("  " ~/ pixel.rep(min = 8, max = 8))
    val small_tile =
      P(tile_num ~/ small_tile_row.rep(min = 8, max = 8, sep = "\n") ~/ "\n")
    val small_tile_set = P(
      small_tile_header ~/
      small_tile_ruler  ~/
      small_tile.rep(min = 64, max = 64)
    )
    val text_tile_header = P("Text Char Tiles\n")
    val text_pixel = P(pixel0 | pixel3)
    val text_tile_row = P("  " ~/ text_pixel.rep(min = 8, max = 8))
    val text_tile =
      P(tile_num ~/ text_tile_row.rep(min = 8, max = 8, sep = "\n") ~/ "\n")
    val text_tile_set = P(
      text_tile_header ~/
      small_tile_ruler  ~/
      text_tile.rep(min = 128, max = 128)
    )
    val parseFile = P(
      Start ~/
      large_tile_set ~/ "\n" ~/ small_tile_set ~/ "\n" ~/ text_tile_set ~/
      End
    )
    parseFile.parse(str) match {
      case Parsed.Success(value, index) => println(s"Success ${value} ${index}")
      case x: Parsed.Failure => println(s"${x}")
    }
  }
}

case class Adder(val base: Int) {
  def add(x: Int): Adder = Adder(base + x)
}
