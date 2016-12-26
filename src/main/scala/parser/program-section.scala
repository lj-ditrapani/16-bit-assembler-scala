package info.ditrapani.asm.parser.program

import info.ditrapani.asm.parser.number.{Number4, Number8}

sealed abstract class Command
sealed abstract class Instruction extends Command {
  def toBinary(): Seq[Byte]
}
sealed abstract class RealInstruction extends Instruction
sealed abstract class PseudoInstruction extends Instruction
final class Label extends Command

final case class IEnd(index: Int) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(0x00.toByte, 0x00.toByte)
}

final case class ImmediateByteInstruction(
    mnemonic: String,
    op_code: Int,
    index: Int,
    immediate_8_bit: Number8,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    op_code | immediate_8_bit.value >> 4,
    (immediate_8_bit.value & 0x0F) << 4 | destination_register.value
  ).map(_.toByte)
}

object ImmediateByteInstruction {
  private def selectMaker
    (mnemonic: String, op_code: Int)
    (parsed_values: (Int, Number8, Number4)): ImmediateByteInstruction = {
    val (index, immediate_8_bit, destination_register) = parsed_values
    ImmediateByteInstruction(
      mnemonic, op_code, index, immediate_8_bit, destination_register
    )
  }

  val hbyMaker = selectMaker("HBY", 0x10) _

  val lbyMaker = selectMaker("LBY", 0x20) _
}

final case class TwoOperandInstruction(
    mnemonic: String,
    op_code: Int,
    index: Int,
    address_register: Number4,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    op_code | address_register.value,
    destination_register.value
  ).map(_.toByte)
}

object TwoOperandInstruction {
  def selectMaker
    (mnemonic: String, op_code: Int)
    (parsed_values: (Int, Number4, Number4)): TwoOperandInstruction = {
    val (index, source_register_1, destination_register) = parsed_values
    TwoOperandInstruction(
      mnemonic, op_code, index, source_register_1, destination_register
    )
  }
}

final case class Str(
    index: Int,
    address_register: Number4,
    value_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    0x40 | address_register.value,
    value_register.value << 4
  ).map(_.toByte)
}

final case class ThreeOperandInstruction(
    mnemonic: String,
    op_code: Int,
    index: Int,
    source_register_1: Number4,
    operand_2: Number4,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    op_code | source_register_1.value,
    operand_2.value << 4 | destination_register.value
  ).map(_.toByte)
}

object ThreeOperandInstruction {
  def selectMaker
    (mnemonic: String, op_code: Int)
    (parsed_values: (Int, Number4, Number4, Number4)): ThreeOperandInstruction = {
    val (index, source_register_1, operand_2, destination_register) = parsed_values
    ThreeOperandInstruction(
      mnemonic, op_code, index, source_register_1, operand_2, destination_register
    )
  }
}

final case class Shf(
    index: Int,
    source_register_1: Number4,
    direction: String,
    ammount: Number4,
    destination_register: Number4
) extends RealInstruction {
  def toBinary(): Seq[Byte] = {
    val is_right = direction match {
      case "R" => 1
      case _ => 0
    }
    val direction_and_ammount = is_right << 3 | ammount.value - 1
    Seq(
      0xD0 | source_register_1.value,
      direction_and_ammount << 4 | destination_register.value
    ).map(_.toByte)
  }
}

final case class ValueConditions(negative: Boolean, zero: Boolean, positive: Boolean) {
  private def bool2int(b: Boolean): Int = b match {
    case true => 1
    case false => 0
  }

  def toNibble: Int = {
    val n = bool2int(negative)
    val z = bool2int(zero)
    val p = bool2int(positive)
    n << 2 | z << 1 | p
  }
}

final case class Brv(
    index: Int,
    value_register: Number4,
    conditions: ValueConditions,
    address_register: Number4
) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    0xE0 | value_register.value,
    address_register.value << 4 | conditions.toNibble
  ).map(_.toByte)
}

final case class Brf()

object ProgramSection {
  import fastparse.all._
  import info.ditrapani.asm.parser.BasicParsers._
  import info.ditrapani.asm.parser.number.NumberParser.{number8bit, number4bit}

  val end = P(Index ~ IgnoreCase("END")).map { IEnd(_) }

  private def immediateByte(mnemonic: String) = P(
    Index ~ IgnoreCase(mnemonic) ~/ spaces ~/ number8bit ~/ spaces ~/ number4bit
  )

  val hby = immediateByte("HBY").map(ImmediateByteInstruction.hbyMaker)

  val lby = immediateByte("LBY").map(ImmediateByteInstruction.lbyMaker)

  private def twoOperand(mnemonic: String) = P(
    Index ~ IgnoreCase(mnemonic) ~/ spaces ~/ number4bit ~/ spaces ~/ number4bit
  )

  val lod = twoOperand("LOD").map(TwoOperandInstruction.selectMaker("LOD", 0x30))

  val str = P(
    Index ~ IgnoreCase("STR") ~/ spaces ~/ number4bit ~/ spaces ~/ number4bit
  ).map(Str.tupled)

  private def threeOperands(mnemonic: String) = P(
    Index ~ IgnoreCase(mnemonic) ~/
    spaces ~/ number4bit ~/
    spaces ~/ number4bit ~/
    spaces ~/ number4bit
  )

  val add = threeOperands("ADD").map(ThreeOperandInstruction.selectMaker("ADD", 0x50))

  val sub = threeOperands("SUB").map(ThreeOperandInstruction.selectMaker("SUB", 0x60))

  val adi = threeOperands("ADI").map(ThreeOperandInstruction.selectMaker("ADI", 0x70))

  val sbi = threeOperands("SBI").map(ThreeOperandInstruction.selectMaker("SBI", 0x80))

  val and = threeOperands("AND").map(ThreeOperandInstruction.selectMaker("AND", 0x90))

  val orr = threeOperands("ORR").map(ThreeOperandInstruction.selectMaker("ORR", 0xA0))

  val xor = threeOperands("XOR").map(ThreeOperandInstruction.selectMaker("XOR", 0xB0))

  val not = twoOperand("NOT").map(TwoOperandInstruction.selectMaker("NOT", 0xC0))

  val number1to8 = number4bit.flatMap(n =>
    (n.value >= 1 && n.value <= 8) match {
      case true => Pass.map(x => n)
      case false => Fail.opaque("SHF shift ammount must be a number in 1-8 inclusive")
    }
  )

  val shf = P(
    Index ~ IgnoreCase("SHF") ~/ spaces ~/ number4bit ~/ spaces ~/
    ("L" | "R").! ~/ number1to8 ~/ number4bit
  ).map(Shf.tupled)

  val value_conditions = (
    P("NZP").map(_ => (true, true, true)) |
    P("NZ").map(_ => (true, true, false)) |
    P("NP").map(_ => (true, false, true)) |
    P("ZP").map(_ => (false, true, true)) |
    P("N").map(_ => (true, false, false)) |
    P("Z").map(_ => (false, true, false)) |
    P("P").map(_ => (false, false, true))
  ).map(ValueConditions.tupled(_))

  val brv = P(
    Index ~ IgnoreCase("BRV") ~/ spaces ~/ number4bit ~/ spaces ~/
    value_conditions ~/ spaces ~/ number4bit
  ).map(Brv.tupled)

  val instruction = P(
    end.|[RealInstruction](hby) | lby | lod | str |
    add | sub | adi | sbi | and | orr | xor | not | shf | brv
  )

  val program_entry = P(optional_spaces ~ instruction ~/ tail_noise ~/ "\n" ~/ noise)
  val program_section = P(
    ".program-rom\n" ~/ noise ~/ program_entry.rep ~/ ".end-program-rom" ~/ tail_noise
  )
}
