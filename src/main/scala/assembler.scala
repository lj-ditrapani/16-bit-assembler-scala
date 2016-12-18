package info.ditrapani.asm

import info.ditrapani.asm.parser.AsmParser

object Assembler {
  def assemble(text: String): Either[String, Seq[Byte]] = {
    AsmParser.parseAsm(text).right.map(
      x => List(65.toByte, 66.toByte, 10.toByte)
    )
  }
}
