package info.ditrapani.asm

import info.ditrapani.asm.parser.AsmParser

object Assembler {
  def assemble(text: String): Either[String, Seq[Byte]] = {
    AsmParser.parseAsm(text)
      .flatMap(symboltable.SymbolTable.fillSymbols)
      .flatMap(binary.BinaryGenerator.generate)
      .map(x => List(65.toByte, 66.toByte, 10.toByte))
  }
}
