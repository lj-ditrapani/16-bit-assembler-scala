package info.ditrapani.asm

object Assembler {
  def apply(text: String): Either[String, Seq[Byte]] = {
    Right(Vector(65, 66, 67).map(_.toByte))
  }
}
