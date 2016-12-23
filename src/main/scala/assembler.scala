package info.ditrapani.asm

import info.ditrapani.asm.parser.AsmParser

object Assembler {
  def assemble(text: String): Either[String, Seq[Byte]] = {
    val parserResult = AsmParser.parseAsm(text)
    val symbolResults = symboltable.SymbolTable.fillSymbols(parserResult)
    val binaryResult = binary.BinaryGenerator.generate(symbolResults)
    binaryResult match {
      case binary.GoodBinaryResult => Right(List(65.toByte, 66.toByte, 10.toByte))
      case binary.BadBinaryResult(message) => Left(message)
    }
  }
}
