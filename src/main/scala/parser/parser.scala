package info.ditrapani.asm.parser

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

    result match {
      case Parsed.Success(value, index) =>
        val (symbol_seq, program_commands, data_commands) = value
        Right(ParserResult(symbol_seq, Seq[program.Command](), Seq[data.Command]()))
      case failure: Parsed.Failure =>
        // Utils.parsedFailure2String(failure)
        val file_type = "assembly"
        val input = failure.extra.input
        val Array(line, column) = input.repr.prettyIndex(input, failure.index).split(":")
        val s = s"Failure parsing $file_type file occured at\n" +
          s"Line: $line\nColumn: $column\n"
        Left(s + failure.msg)
    }
  }
}

final case class ParserResult(
    symbol_entries: Seq[symbols.SymbolEntry],
    program_commands: Seq[program.Command],
    data_commands: Seq[data.Command]
)
