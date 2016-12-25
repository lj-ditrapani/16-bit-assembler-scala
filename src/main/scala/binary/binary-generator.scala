package info.ditrapani.asm.binary

import info.ditrapani.asm.symboltable.SymbolResults

object BinaryGenerator {
  def generate(symbolResults: SymbolResults): Either[String, BinaryResult] =
    Right(BinaryResult(symbolResults.instructions.flatMap(_.toBinary())))
}

final case class BinaryResult(bytes: Seq[Byte])
