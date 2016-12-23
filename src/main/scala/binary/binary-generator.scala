package info.ditrapani.asm.binary

import info.ditrapani.asm.symboltable.{SymbolResults, GoodSymbolResults, BadSymbolResults}

object BinaryGenerator {
  def generate(symbolResults: SymbolResults): BinaryResult = symbolResults match {
    case GoodSymbolResults => GoodBinaryResult
    case BadSymbolResults(message) => BadBinaryResult(message)
  }
}

sealed abstract class BinaryResult
final case class BadBinaryResult(message: String) extends BinaryResult
final object GoodBinaryResult extends BinaryResult
