package info.ditrapani.asm

import info.ditrapani.asm.parser.AsmParser

object Assembler {
  def assemble(text: String): Either[String, Seq[Byte]] = {
    // parse
    // val eitherParserdResult = AsmParser.parseAsm(text) match {
    AsmParser.parseAsm(text) match {
      case p: parser.GoodParserResult => Right(List(65.toByte, 66.toByte, 10.toByte))
      case parser.BadParserResult(message) => Left(message)
    }
    // fill out symbol table
    // val eitherSymbolResults = symbols.fillSymbols(eitherParserdResult)
    // generate binary
    // generateBinary(eitherSymbolResults)
  }
}
