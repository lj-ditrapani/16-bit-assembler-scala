package info.ditrapani.asm

object Assembler {
  def apply(text: String): Either[String, Seq[Byte]] = {
    import fastparse.all._

    val valid_cahrs = P(
      Start ~/ ("\n" | CharIn('\u0020' to '\u007E')).rep ~/ End
    )
    println(valid_cahrs.parse(text))

    val comment = P("#" ~/ CharsWhile(_ != '\n', min = 0))
    val spaces = P(" ".rep(1))

    val tail_noise = P((spaces ~/ comment) | " ".rep)

    val noise = P(" ".rep ~ comment.? ~ "\n").rep

    val digit = P(CharIn('0' to '9'))
    val uppercase = P(CharIn('A' to 'Z'))
    val lowercase = P(CharIn('a' to 'z'))
    val letter = P(lowercase | uppercase)
    val number = P(digit.rep(1).!.map(_.toInt))
    val symbol = P(letter ~/ (letter | digit | "-" | "_").rep(min = 0)).!
    val symbol_entry = P(
      noise ~/ " ".rep ~/ symbol ~/ spaces ~/ number ~/ tail_noise
    )
    val symbols_section = P(
      ".symbols\n" ~/ Index ~/ symbol_entry.rep ~/ noise ~/ ".end-symbols"
    )
    val file = P(Start ~/ noise ~/ symbols_section)
    // Start ~/ noise ~/ symbols_section ~/
    //   noise ~/ data_section ~/
    //   noise ~/ program_section ~/
    //   noise ~/ End
    //   command = command_stuff ~/ P(tail_noise ~ "\n")
    val s = file.parse(text) match {
      case f: Parsed.Failure => f.toString + "\n" + f.extra.traced.trace.split("/").mkString("\n")
      case s: Parsed.Success[Int] => s.toString
    }
    println(s)
    Right(Vector(65, 66, 67).map(_.toByte))
  }
}
