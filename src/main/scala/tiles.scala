package info.ditrapani.asm.tiles

object Tiles {
  def parseStr(str: String): Either[String, Seq[Byte]] = {
    import fastparse.all._

    val tile_ruler = P("  0 1 2 3 4 5 6 7\n")
    val pixel0 = P("[]").map((x) => 0)
    val pixel1 = P("<>").map((x) => 1)
    val pixel = P(pixel0 | pixel1)
    val tile_row = P("  " ~/ pixel.rep(min = 8, max = 8)).map(Tile.row)
    val tile_num = P(CharIn("0123456789ABCDEF").rep(min = 2, max = 2) ~/ "\n")
    val tile = P(
      tile_num ~/ tile_row.rep(min = 12, max = 12, sep = "\n") ~/ "\n"
    )
    val tile_set = P(
      tile_ruler ~/
      tile.rep(min = 256, max = 256)
    ).map(_.flatten)
    val parseFile = P(
      Start ~/ tile_set ~/ End
    )
    parseFile.parse(str) match {
      case Parsed.Success(value, index) => Right(value)
      case x: Parsed.Failure => Left(s"${x}")
    }
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

object Tile {
  def row(seq: Seq[Int]): Byte = {
    assert(seq.length == 8)
    val unsummed_vals: Seq[Int] =
      for ((v: Int, i: Int) <- seq.zipWithIndex) yield {
        v << (7 - i)
      }
    val byte_val: Int = unsummed_vals.sum
    byte_val.toByte
  }
}
