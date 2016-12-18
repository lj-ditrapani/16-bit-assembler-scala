package info.ditrapani.asm.parser

import info.ditrapani.asm.Utils

object AsmParser {
  import fastparse.all._

  type ParserResult = Either[
    String,
    (
      Seq[symbols.SymbolEntry],
      Seq[program.Command]
    )
  ]

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
    symbols.SymbolsSection.symbols_section ~/ noise ~/
    program_section ~/ noise ~/
    video_section ~/ noise ~/
    data_section ~/ noise ~/
    End
  )
  */

  def parseAsm(text: String): ParserResult = {
    val valid_cahrs = P(
      Start ~/ ("\n" | CharIn('\u0020' to '\u007E')).rep ~/ End
    )
    def parseFile(x: Unit): ParserResult = {
      // Utils.parsedResult2Either[Seq[Byte]]("assembly", file.parse(text))
      val parsed_symbols = symbols.SymbolsSection.symbols_section.parse(text)
      Utils.parsedResult2Either[Seq[symbols.SymbolEntry]]("assembly", parsed_symbols)
        .right.map((symbol_seq) => (symbol_seq, Seq[program.Command]()))
    }

    val result = valid_cahrs.parse(text)
    Utils.parsedResult2Either[Unit]("assembly", result).flatMap(parseFile)
  }
}
