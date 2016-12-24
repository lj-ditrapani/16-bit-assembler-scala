package info.ditrapani.asm.binary

import info.ditrapani.asm.symboltable.SymbolResults

object BinaryGenerator {
  def generate(symbolResults: SymbolResults): Either[String, BinaryResult] =
    Right(BinaryResult(List(65.toByte, 66.toByte, 10.toByte)))
}

final case class BinaryResult(bytes: Seq[Byte])
