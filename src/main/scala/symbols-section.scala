package info.ditrapani.asm

import info.ditrapani.asm.number.Number16
// import info.ditrapani.asm.number.NumberParser.number16bit

object SymbolsSection {
  /*
  import fastparse.all._
  import BasicParsers._

  val symbol_entry = P(
    Index ~ symbol ~/ spaces ~/ number16bit ~/ tail_noise
  ).map { case (index, symbol, number16) => SymbolEntry(index, symbol, Right(number16)) }
  val symbol_section_line = P(" ".rep ~ (comment | symbol_entry).? ~ "\n")
  val symbols_section = P(
    ".symbols\n" ~/ symbol_section_line.rep ~/ ".end-symbols" ~/ tail_noise
  )
  */

  type SymbolValue = Either[String, Number16]

  final case class SymbolEntry(index: Int, key: String, value: SymbolValue)
}
