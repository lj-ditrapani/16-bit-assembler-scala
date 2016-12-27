package info.ditrapani.asm.parser

import info.ditrapani.asm.Utils

object AsmParser {
  type ParserResultTuple = (
    Seq[symbols.SymbolEntry],
    Seq[program.Command],
    Seq[data.Command]
  )
  type RawParserResult = Either[String, ParserResultTuple]

  val file = {
    import fastparse.all._
    import info.ditrapani.asm.parser.BasicParsers._
    P(
      Start ~/ noise ~/
      symbols.SymbolsSection.symbols_section ~/ noise ~/
      program.ProgramSection.program_section ~/ noise ~/
      video.VideoSection.video_section ~/ noise ~/
      data.DataSection.data_section ~/ noise ~/
      empty_text ~/
      End
    )
  }

  def parseAsm(text: String): Either[String, ParserResult] = {
    import fastparse.all._

    val valid_cahrs = P(
      Start ~/ ("\n" | CharIn('\u0020' to '\u007E')).rep ~/ End
    )

    val result = valid_cahrs.parse(text) match {
      case Parsed.Success(value, index) => file.parse(text)
      case failure: Parsed.Failure => failure
    }

    Utils.parsedResult2Either("assembly", result).right.map((value) => {
      val (symbol_seq, program_commands, data_commands) = value
      ParserResult(symbol_seq, program_commands, data_commands)
    })
  }
}

final case class ParserResult(
    symbol_entries: Seq[symbols.SymbolEntry],
    program_commands: Seq[program.Command],
    data_commands: Seq[data.Command]
)
