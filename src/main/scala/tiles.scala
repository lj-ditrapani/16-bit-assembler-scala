package info.ditrapani.asm.tiles

object Tiles {
  def parseStr(str: String): Either[String, Seq[Byte]] = {
    import fastparse.all._

    val tile_ruler = P("  0 1 2 3 4 5 6 7\n")
    val pixel0 = P("[]").map((x) => 0)
    val pixel1 = P("<>").map((x) => 1)
    val pixel = P(pixel0 | pixel1)
    val tile_row = P("  " ~/ pixel.rep(exactly = 8)).map(Tile.row)
    val tile_num = {
      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      var i = 0
      P(CharIn("0123456789ABCDEF").rep(exactly = 2).!.flatMap( actual_str => {
        val temp = i
        i += 1
        val actual = Integer.parseInt(actual_str, 16)
        (actual == temp) match {
          case false =>
            val expected_hex = Integer.toHexString(temp).reverse.padTo(2, '0').reverse
            Fail.opaque(s"[Number should be $expected_hex, but was $actual_str]")
          case true => Pass
        }
      }) ~/ "\n")
    }
    val tile = P(
      tile_num ~/ tile_row.rep(exactly = 12, sep = "\n") ~/ "\n"
    )
    val tile_set = P(
      tile_ruler ~/
      tile.rep(exactly = 256)
    ).map(_.flatten)
    val parseFile = P(
      Start ~/ tile_set ~/ End
    )
    parseFile.parse(str) match {
      case Parsed.Success(value, index) =>
        Right(value)
      case failure: Parsed.Failure =>
        val input = failure.extra.input
        val Array(line, column) = input.repr.prettyIndex(input, failure.index).split(":")
        val s = s"Failure parsing ASCII tile file occured at\nLine: $line\nColumn: $column\n"
        Left(s + failure.msg)
    }
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
