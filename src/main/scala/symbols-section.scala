package info.ditrapani.asm.symbols

import info.ditrapani.asm.number.Number16
import info.ditrapani.asm.number.NumberParser.number16bit

object SymbolsSection {
  import fastparse.all._
  import info.ditrapani.asm.BasicParsers._

  val predefined_symbols = {
    val number_registers = for {
      i <- 0 to 15
    } yield (s"R${i}" -> Number16(i))
    val letter_registers = for {
      i <- 0 to 5
    } yield (s"R${(65 + i).toChar}" -> Number16(10 + i))
    val others = List(
      ("video-cells" -> Number16(0xFC00)),
      ("video-enable" -> Number16(0xFE80)),
      ("gamepad" -> Number16(0xFFFD)),
      ("frame-interrupt-enable" -> Number16(0xFFFE)),
      ("frame-interrupt-vector" -> Number16(0xFFFF))
    )
    val all_pairs = number_registers ++ letter_registers ++ others
    Map(all_pairs: _*)
  }

  val symbol_entry = P(
    optional_spaces ~ Index ~ symbol ~/ spaces ~/
    number16bit ~/ tail_noise ~/ "\n" ~/ noise
  ).map { case (index, symbol, number16) => SymbolEntry(index, symbol, number16) }
  val symbols_section = P(
    ".symbols\n" ~/ noise ~/ symbol_entry.rep ~/ ".end-symbols" ~/ tail_noise
  )

  final case class SymbolEntry(index: Int, key: String, value: Number16)
}
