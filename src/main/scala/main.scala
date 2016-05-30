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
    val large_tile_row = P("  " ~/ pixel.rep(min = 16, max = 16)).map(Large.row)
    val tile_num = P(CharIn("0123456789ABCDEF").rep(min = 2, max = 2) ~/ "\n")
    val large_tile = P(
      tile_num ~/ large_tile_row.rep(min = 16, max = 16, sep = "\n") ~/ "\n"
    ).map(_.flatten)
    val large_tile_set = P(
      large_tile_header ~/
      large_tile_ruler ~/
      large_tile.rep(min = 64, max = 64)
    ).map(_.flatten)
    val small_tile_header = P("Small Tiles\n")
    val small_tile_ruler = P("  0 1 2 3 4 5 6 7\n")
    val small_tile_row = P("  " ~/ pixel.rep(min = 8, max = 8)).map(Small.row)
    val small_tile = P(
      tile_num ~/ small_tile_row.rep(min = 8, max = 8, sep = "\n") ~/ "\n"
    ).map(_.flatten)
    val small_tile_set = P(
      small_tile_header ~/
      small_tile_ruler  ~/
      small_tile.rep(min = 64, max = 64)
    ).map(_.flatten)
    val text_tile_header = P("Text Char Tiles\n")
    val text_pixel0 = P("[]").map((x) => 0)
    val text_pixel1 = P("<>").map((x) => 1)
    val text_pixel = P(text_pixel0 | text_pixel1)
    val text_tile_row =
      P("  " ~/ text_pixel.rep(min = 8, max = 8)).map(Text.row)
    val text_tile =
      P(tile_num ~/ text_tile_row.rep(min = 8, max = 8, sep = "\n") ~/ "\n")
    val text_tile_set = P(
      text_tile_header ~/
      small_tile_ruler  ~/
      text_tile.rep(min = 128, max = 128)
    ).map(_.flatten)
    val parseFile = P(
      Start ~/
      large_tile_set ~/ "\n" ~/ small_tile_set ~/ "\n" ~/ text_tile_set ~/
      End
    ).map((x) => x._1 ++ x._2 ++ x._3)
    parseFile.parse(str) match {
      case Parsed.Success(value, index) => toFile(value)
      case x: Parsed.Failure => println(s"${x}")
    }
  }

  def toFile(s: Seq[Byte]): Unit = {
    import java.io.FileOutputStream

    println(s"Success ${s}")
    new FileOutputStream("tiles.bin").write(s.toArray)
  }
}

object Utils {
  def fourPixels2oneByte(s: Seq[Int]): Byte = {
    assert(s.size == 4)
    ((s(0) << 6) + (s(1) << 4) + (s(2) << 2) + s(3)).toByte
  }
  def eightPixels2twoBytes(seq: Seq[Int]): Seq[Byte] = {
    assert(seq.size == 8)
    val (seq1, seq2) = seq.splitAt(4)
    Seq(fourPixels2oneByte(seq1), fourPixels2oneByte(seq2))
  }
}

object Large {
  type Row = Seq[Byte]

  def row(seq: Seq[Int]): Row = {
    val (seq1, seq2) = seq.splitAt(8)
    Utils.eightPixels2twoBytes(seq1) ++ Utils.eightPixels2twoBytes(seq2)
  }
}

object Small {
  type Row = Seq[Byte]

  def row(seq: Seq[Int]): Row = {
    Utils.eightPixels2twoBytes(seq)
  }
}

object Text {
  type Tile = Vector[Byte]
  def row(seq: Seq[Int]): Byte = {
    val unsummed_vals: Seq[Int] =
      for ((v: Int, i: Int) <- seq.zipWithIndex) yield {
        v << (7 - i)
      }
    val byte_val: Int = unsummed_vals.sum
    byte_val.toByte
  }
}
