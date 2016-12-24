package info.ditrapani.asm

import info.ditrapani.asm.parser.AsmParser

object Assembler {
  def assemble(text: String): Either[String, Seq[Byte]] = {
    AsmParser.parseAsm(text)
      .flatMap(symboltable.SymbolTable.fillSymbols)
      .flatMap(binary.BinaryGenerator.generate)
      .map(binaryResult => binaryResult.bytes)
  }
}
