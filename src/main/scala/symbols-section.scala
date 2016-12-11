package info.ditrapani.asm

import info.ditrapani.asm.number.Number16
import info.ditrapani.asm.number.NumberParser.number16bit

object SymbolsSection {
  import fastparse.all._
  import BasicParsers._

  val symbol_entry = P(
    optional_spaces ~ Index ~ symbol ~/ spaces ~/ number16bit ~/ tail_noise ~/ "\n" ~/ noise
  ).map { case (index, symbol, number16) => SymbolEntry(index, symbol, Right(number16)) }
  val symbols_section = P(
    ".symbols\n" ~/ noise ~/ symbol_entry.rep ~/ ".end-symbols" ~/ tail_noise
  )

  type SymbolValue = Either[String, Number16]

  final case class SymbolEntry(index: Int, key: String, value: SymbolValue)
}
