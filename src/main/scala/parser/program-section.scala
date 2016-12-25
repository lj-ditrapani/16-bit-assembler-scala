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

final case class Hby(
    index: Int,
    immediate_8_bit: Number8,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    0x10 | immediate_8_bit.value >> 4,
    (immediate_8_bit.value & 0x0F) << 4 | destination_register.value
  ).map(_.toByte)
}

final case class Lby(
    index: Int,
    immediate_8_bit: Number8,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    0x20 | immediate_8_bit.value >> 4,
    (immediate_8_bit.value & 0x0F) << 4 | destination_register.value
  ).map(_.toByte)
}

final case class Lod(
    index: Int,
    address_register: Number4,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    0x30 | address_register.value,
    destination_register.value
  ).map(_.toByte)
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
    (parsed_values: (Int, (Number4, Number4, Number4))): ThreeOperandInstruction = {
    val (index, (source_register_1, operand_2, destination_register)) = parsed_values
    ThreeOperandInstruction(
      mnemonic, op_code, index, source_register_1, operand_2, destination_register
    )
  }
}

object ProgramSection {
  import fastparse.all._
  import info.ditrapani.asm.parser.BasicParsers._
  import info.ditrapani.asm.parser.number.NumberParser.{number8bit, number4bit}

  val end = P(Index ~ IgnoreCase("END")).map { IEnd(_) }

  val hby = P(
    Index ~ IgnoreCase("HBY") ~/ spaces ~/ number8bit ~/ spaces ~/ number4bit
  ).map {
    case (index, immediate_8_bit, destination_register) =>
      Hby(index, immediate_8_bit, destination_register)
  }

  val lby = P(
    Index ~ IgnoreCase("LBY") ~/ spaces ~/ number8bit ~/ spaces ~/ number4bit
  ).map {
    case (index, immediate_8_bit, destination_register) =>
      Lby(index, immediate_8_bit, destination_register)
  }

  val lod = P(
    Index ~ IgnoreCase("LOD") ~/ spaces ~/ number4bit ~/ spaces ~/ number4bit
  ).map {
    case (index, address_register, destination_register) =>
      Lod(index, address_register, destination_register)
  }

  val str = P(
    Index ~ IgnoreCase("STR") ~/ spaces ~/ number4bit ~/ spaces ~/ number4bit
  ).map {
    case (index, address_register, value_register) =>
      Str(index, address_register, value_register)
  }

  val three_operands =
    spaces ~/ number4bit ~/ spaces ~/ number4bit ~/spaces ~/number4bit

  val add = P(Index ~ IgnoreCase("ADD") ~/ three_operands)
    .map(ThreeOperandInstruction.selectMaker("ADD", 0x50))

  val sub = P(Index ~ IgnoreCase("SUB") ~/ three_operands)
    .map(ThreeOperandInstruction.selectMaker("SUB", 0x60))

  val adi = P(Index ~ IgnoreCase("ADI") ~/ three_operands)
    .map(ThreeOperandInstruction.selectMaker("ADI", 0x70))

  val sbi = P(Index ~ IgnoreCase("SBI") ~/ three_operands)
    .map(ThreeOperandInstruction.selectMaker("SBI", 0x80))

  val and = P(Index ~ IgnoreCase("AND") ~/ three_operands)
    .map(ThreeOperandInstruction.selectMaker("AND", 0x90))

  val orr = P(Index ~ IgnoreCase("ORR") ~/ three_operands)
    .map(ThreeOperandInstruction.selectMaker("ORR", 0xA0))

  val xor = P(Index ~ IgnoreCase("XOR") ~/ three_operands)
    .map(ThreeOperandInstruction.selectMaker("XOR", 0xB0))

  val instruction = P(
    end.|[RealInstruction](hby) | lby | lod | str |
    add | sub | adi | sbi | and | orr | xor
  )

  val program_entry = P(optional_spaces ~ instruction ~/ tail_noise ~/ "\n" ~/ noise)
  val program_section = P(
    ".program-rom\n" ~/ noise ~/ program_entry.rep ~/ ".end-program-rom" ~/ tail_noise
  )
}
