package info.ditrapani.asm.parser.data

import info.ditrapani.asm.parser.number.NumberParser.number16bit

object DataSection {
  import fastparse.all._
  import info.ditrapani.asm.parser.BasicParsers._

  val data_entry = P(
    optional_spaces ~ Index ~ "word" ~/ spaces ~/ symbol ~/ spaces ~/ number16bit ~/
    tail_noise ~/ "\n" ~/ noise
  )
  val data_section = P(
    ".data-ram\n" ~/ noise ~/ data_entry.rep ~/ ".end-data-ram" ~/ tail_noise
  )
}
