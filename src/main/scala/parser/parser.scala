package info.ditrapani.asm.parser

import info.ditrapani.asm.Utils

object AsmParser {

  type ParserResult = Either[
    String,
    (
      Seq[symbols.SymbolEntry],
      Seq[program.Command]
    )
  ]

  val file = {
    import fastparse.all._
    import info.ditrapani.asm.parser.BasicParsers._
    P(
      Start ~/ noise ~/
      symbols.SymbolsSection.symbols_section ~/ noise ~/
      program.ProgramSection.program_section ~/ noise ~/
      video.VideoSection.video_section ~/ noise ~/
      data.DataSection.data_section ~/ noise ~/
      End
    )
  }

  def parseAsm(text: String): ParserResult = {
    import fastparse.all._

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
