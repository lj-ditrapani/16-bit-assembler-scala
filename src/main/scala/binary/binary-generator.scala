package info.ditrapani.asm.binary

import info.ditrapani.asm.symboltable.SymbolResults

object BinaryGenerator {
  def generate(symbolResults: SymbolResults): Either[String, BinaryResult] =
    Right(BinaryResult())
}

final case class BinaryResult()
