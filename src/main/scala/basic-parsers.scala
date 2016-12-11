package info.ditrapani.asm

object BasicParsers {
  import fastparse.all._

  val comment = P("#" ~/ CharsWhile(_ != '\n', min = 0))
  val optional_spaces = " ".rep
  val spaces = P(" ".rep(1))
  val tail_noise = P((spaces ~/ comment) | optional_spaces)
  val noise = P(optional_spaces ~ comment.? ~ "\n").rep

  val symbol = {
    val digit = P(CharIn('0' to '9'))
    val uppercase = P(CharIn('A' to 'Z'))
    val lowercase = P(CharIn('a' to 'z'))
    val letter = P(lowercase | uppercase)
    P(letter.! ~ (letter | digit | "-" | "_").rep.!).map(makeSymbol)
  }

  private def makeSymbol(tuple: (String, String)): String = tuple._1 + tuple._2
}
