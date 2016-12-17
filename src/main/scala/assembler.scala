package info.ditrapani.asm

import info.ditrapani.asm.parser.AsmParser

object Assembler {
  def assemble(text: String): Either[String, Seq[Byte]] = {
    AsmParser.parse_asm(text)
  }
}
