package info.ditrapani.asm.symboltable

import info.ditrapani.asm.parser.number.Number16
import info.ditrapani.asm.parser.ParserResult
import info.ditrapani.asm.parser.program.Instruction

object SymbolTable {
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

  def fillSymbols(parserResult: ParserResult): Either[String, SymbolResults] = {
    val instructions = parserResult.program_commands.collect { case i: Instruction => i }
    Right(SymbolResults(instructions))
  }
}

final case class SymbolResults(instructions: Seq[Instruction])
