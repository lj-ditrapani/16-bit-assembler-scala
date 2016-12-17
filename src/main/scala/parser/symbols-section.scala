package info.ditrapani.asm.parser.symbols

import info.ditrapani.asm.parser.number.Number16
import info.ditrapani.asm.parser.number.NumberParser.number16bit

object SymbolsSection {
  import fastparse.all._
  import info.ditrapani.asm.parser.BasicParsers._

  val symbol_entry = P(
    optional_spaces ~ Index ~ symbol ~/ spaces ~/
    number16bit ~/ tail_noise ~/ "\n" ~/ noise
  ).map { case (index, symbol, number16) => SymbolEntry(index, symbol, number16) }
  val symbols_section = P(
    ".symbols\n" ~/ noise ~/ symbol_entry.rep ~/ ".end-symbols" ~/ tail_noise
  )

  final case class SymbolEntry(index: Int, key: String, value: Number16)
}
