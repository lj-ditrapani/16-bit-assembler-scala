package info.ditrapani.asm

object Assembler {
  def apply(text: String): Result = {
    import fastparse.all._

    val valid_cahrs = P(
      Start ~/ ("\n" | CharIn('\u0020' to '\u007E')).rep ~/ End
    )
    println(valid_cahrs.parse(text)) // scalastyle:ignore regex

    val comment = P("#" ~/ CharsWhile(_ != '\n', min = 0))
    val spaces = P(" ".rep(1))

    val tail_noise = P((spaces ~/ comment) | " ".rep)

    val noise = P(" ".rep ~ comment.? ~ "\n").rep

    val digit = P(CharIn('0' to '9'))
    val uppercase = P(CharIn('A' to 'Z'))
    val lowercase = P(CharIn('a' to 'z'))
    val letter = P(lowercase | uppercase)
    val number = P(digit.rep(1).!.map(_.toInt))
    val symbol = P(letter.! ~ (letter | digit | "-" | "_").rep.!).map((x) => {
      println(s"Got a symbol ${x._1 + x._2}") // scalastyle:ignore regex
      x._1 + x._2
    })
    val symbol_entry = P(
      Index ~ symbol ~/ spaces ~/ number ~/ tail_noise
    ).map((x) => SymbolEntry(x._1, x._2, Right(x._3)))
    val symbol_section_line = P((" ".rep ~ (comment | symbol_entry).? ~ "\n"))
    val symbols_section = P(
      ".symbols\n" ~/ symbol_section_line.rep ~/ ".end-symbols" ~/ tail_noise
    )
    val data_entry = P(
      Index ~ "word" ~/ spaces ~/ symbol ~/ spaces ~/ number ~/ tail_noise
    )
    val data_section_line = P(" ".rep ~ (comment | data_entry).? ~ "\n")
    val data_section = P(
      ".data\n" ~/ data_section_line.rep ~/ ".end-data" ~/ tail_noise
    )
    val program_entry = P(Index ~ symbol.rep(3, sep = spaces) ~/ tail_noise)
    val program_section_line = P(" ".rep ~ (comment | program_entry).? ~ "\n")
    val program_section = P(
      ".program\n" ~/ program_section_line.rep ~/ ".end-program" ~/ tail_noise
    )
    val file = P(
      Start ~/ noise ~/
      symbols_section ~/ noise ~/
      data_section ~/ noise ~/
      program_section ~/ noise ~/
      End
    )
    val s = file.parse(text) match {
      case f: Parsed.Failure =>
        f.toString + "\n" + f.extra.traced.trace.split("/").mkString("\n")
      case s: Parsed.Success[Int] =>
        s.toString
    }
    println(s) // scalastyle:ignore regex
    Good(Vector(65, 66, 67).map(_.toByte))
  }

  type SymbolValue = Either[String, Int]

  case class SymbolEntry(index: Int, key: String, value: SymbolValue)
}

object NumberParser {
  import fastparse.all._

  // validate number?  .filter  but concerned about error message

  val decimal_digit = P(CharIn('0' to '9'))
  val hex_letters = P(CharIn('a' to 'f') | CharIn('A' to 'F'))
  val hex_digit = P(decimal_digit | hex_letters)
  val binary_digit = P(CharIn("01"))
  val decimal = (("+" | "-").? ~ decimal_digit ~/ (decimal_digit | "_").rep)
  val hex = P("$" ~/ hex_digit ~/ (hex_digit | "_").rep)
  val binary = P("%" ~/ binary_digit ~/ (binary_digit | "_").rep)
  val number = (decimal | hex | binary)
}
