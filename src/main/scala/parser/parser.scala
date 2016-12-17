package info.ditrapani.asm.parser

import info.ditrapani.asm.Utils
import fastparse.all._

object AsmParser {
  /*
  val data_entry = P(
    Index ~ "word" ~/ spaces ~/ symbol ~/ spaces ~/ number16bit ~/ tail_noise
  )
  val data_section_line = P(" ".rep ~ (comment | data_entry).? ~ "\n")
  val data_section = P(
    ".data-ram\n" ~/ data_section_line.rep ~/ ".end-data-ram" ~/ tail_noise
  )
  val program_entry = P(Index ~ symbol.rep(3, sep = spaces) ~/ tail_noise)
  val program_section_line = P(" ".rep ~ (comment | program_entry).? ~ "\n")
  val program_section = P(
    ".program-rom\n" ~/ program_section_line.rep ~/ ".end-program-rom" ~/ tail_noise
  )
  val file = P(
    Start ~/ noise ~/
    symbols_section ~/ noise ~/
    program_section ~/ noise ~/
    video_section ~/ noise ~/
    data_section ~/ noise ~/
    End
  )
  */

  def parse_asm(text: String): Either[String, Seq[Byte]] = {

    val valid_cahrs = P(
      Start ~/ ("\n" | CharIn('\u0020' to '\u007E')).rep ~/ End
    )
    def parseFile(x: Unit): Either[String, Seq[Byte]] = {
      // Utils.parsedResult2Either[Seq[Byte]]("assembly", file.parse(text))
      Right(List(65.toByte, 66.toByte, 10.toByte))
    }

    val result = valid_cahrs.parse(text)
    Utils.parsedResult2Either[Unit]("assembly", result).flatMap(parseFile)
  }
}
