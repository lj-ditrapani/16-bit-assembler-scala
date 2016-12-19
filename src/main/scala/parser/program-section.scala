package info.ditrapani.asm.parser.program

sealed abstract class Command

final class Instruction extends Command

final class Label extends Command

object ProgramSection {
  import fastparse.all._
  import info.ditrapani.asm.parser.BasicParsers._

  val program_entry = P(
    optional_spaces ~ Index ~ symbol.rep(min = 2, max = 3, sep = spaces) ~/
    tail_noise ~/ "\n" ~/ noise
  )
  val program_section = P(
    ".program-rom\n" ~/ noise ~/
    program_entry.rep ~/
    ".end-program-rom" ~/ tail_noise
  )
}
