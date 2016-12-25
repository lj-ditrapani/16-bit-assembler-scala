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

sealed abstract class ThreeOperand(
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

final case class Add(
    index: Int,
    source_register_1: Number4,
    source_register_2: Number4,
    destination_register: Number4) extends ThreeOperand(
      0x50, index, source_register_1, source_register_2, destination_register)

final case class Sub(
    index: Int,
    source_register_1: Number4,
    source_register_2: Number4,
    destination_register: Number4) extends ThreeOperand(
      0x60, index, source_register_1, source_register_2, destination_register)

final case class Adi(
    index: Int,
    source_register_1: Number4,
    immediate_4_bit: Number4,
    destination_register: Number4) extends ThreeOperand(
      0x70, index, source_register_1, immediate_4_bit, destination_register)

final case class Sbi(
    index: Int,
    source_register_1: Number4,
    immediate_4_bit: Number4,
    destination_register: Number4) extends ThreeOperand(
      0x80, index, source_register_1, immediate_4_bit, destination_register)

final case class And(
    index: Int,
    source_register_1: Number4,
    source_register_2: Number4,
    destination_register: Number4) extends ThreeOperand(
      0x90, index, source_register_1, source_register_2, destination_register)

final case class Orr(
    index: Int,
    source_register_1: Number4,
    source_register_2: Number4,
    destination_register: Number4) extends ThreeOperand(
      0xA0, index, source_register_1, source_register_2, destination_register)

final case class Xor(
    index: Int,
    source_register_1: Number4,
    source_register_2: Number4,
    destination_register: Number4) extends ThreeOperand(
      0xB0, index, source_register_1, source_register_2, destination_register)

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

  def to_three_operand_instruction[T <: ThreeOperand](
      operands: (Int, (Number4, Number4, Number4)),
      constructor: (Int, Number4, Number4, Number4) => T
  ): T = {
     val (index, (source_register_1, source_register_2, destination_register)) = operands
    constructor(index, source_register_1, source_register_2, destination_register)
  }

  val add = P(Index ~ IgnoreCase("ADD") ~/ three_operands)
    .map(to_three_operand_instruction(_, Add.apply _))

  val sub = P(Index ~ IgnoreCase("SUB") ~/ three_operands)
    .map(to_three_operand_instruction(_, Sub.apply _))

  val adi = P(Index ~ IgnoreCase("ADI") ~/ three_operands)
    .map(to_three_operand_instruction(_, Adi.apply _))

  val sbi = P(Index ~ IgnoreCase("SBI") ~/ three_operands)
    .map(to_three_operand_instruction(_, Sbi.apply _))

  val and = P(Index ~ IgnoreCase("AND") ~/ three_operands)
    .map(to_three_operand_instruction(_, And.apply _))

  val orr = P(Index ~ IgnoreCase("ORR") ~/ three_operands)
    .map(to_three_operand_instruction(_, Orr.apply _))

  val xor = P(Index ~ IgnoreCase("XOR") ~/ three_operands)
    .map(to_three_operand_instruction(_, Xor.apply _))

  val instruction = P(end |[RealInstruction] hby | lby | lod | str | add)

  val program_entry = P(optional_spaces ~ instruction ~/ tail_noise ~/ "\n" ~/ noise)
  val program_section = P(
    ".program-rom\n" ~/ noise ~/ program_entry.rep ~/ ".end-program-rom" ~/ tail_noise
  )
}
