package info.ditrapani.asm.parser.data

import info.ditrapani.asm.parser.number.Number16
import info.ditrapani.asm.parser.number.NumberParser.number16bit

object DataSection {
  import fastparse.all._
  import info.ditrapani.asm.parser.BasicParsers._

  val word = P(Index ~ "word" ~/ spaces ~/ symbol ~/ spaces ~/ number16bit).map {
    case (index, name, value) => Word(index, name, value)
  }

  val move = P(Index ~ "move" ~/ spaces ~/ number16bit).map {
    case (index, address) => Move(index, address)
  }

  val command = P(word.|[Command](move))

  val data_entry = P(
    optional_spaces ~ command ~/
    tail_noise ~/ "\n" ~/ noise
  )
  val data_section = P(
    ".data-ram\n" ~/ noise ~/ data_entry.rep ~/ ".end-data-ram" ~/ tail_noise
  )
}

sealed abstract class Command
final case class Word(index: Int, name: String, value: Number16) extends Command
final case class Move(index: Int, address: Number16) extends Command
