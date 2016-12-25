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
    immediate8bit: Number8,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    0x10 | immediate8bit.value >> 4,
    (immediate8bit.value & 0x0F) << 4 | destination_register.value
  ).map(_.toByte)
}

final case class Lby(
    index: Int,
    immediate8bit: Number8,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    0x20 | immediate8bit.value >> 4,
    (immediate8bit.value & 0x0F) << 4 | destination_register.value
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

final case class Add(
    index: Int,
    source_register_1: Number4,
    source_register_2: Number4,
    destination_register: Number4) extends RealInstruction {
  def toBinary(): Seq[Byte] = Seq(
    0x50 | source_register_1.value,
    source_register_2.value << 4 | destination_register.value
  ).map(_.toByte)
}

object ProgramSection {
  import fastparse.all._
  import info.ditrapani.asm.parser.BasicParsers._
  import info.ditrapani.asm.parser.number.NumberParser.{number8bit, number4bit}

  val end = P(Index ~ IgnoreCase("END")).map { IEnd(_) }

  val hby = P(
    Index ~ IgnoreCase("HBY") ~/ spaces ~/ number8bit ~/ spaces ~/ number4bit
  ).map {
    case (index, immediate8bit, destination_register) =>
      Hby(index, immediate8bit, destination_register)
  }

  val lby = P(
    Index ~ IgnoreCase("LBY") ~/ spaces ~/ number8bit ~/ spaces ~/ number4bit
  ).map {
    case (index, immediate8bit, destination_register) =>
      Lby(index, immediate8bit, destination_register)
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

  val add = P(
    Index ~ IgnoreCase("ADD") ~/ spaces ~/
    number4bit ~/ spaces ~/ number4bit ~/spaces ~/number4bit
  ).map {
    case (index, source_register_1, source_register_2, destination_register) =>
      Add(index, source_register_1, source_register_2, destination_register)
  }

  val instruction = P(end | hby.|[RealInstruction](lby) | lod | str | add)

  val program_entry = P(optional_spaces ~ instruction ~/ tail_noise ~/ "\n" ~/ noise)
  val program_section = P(
    ".program-rom\n" ~/ noise ~/ program_entry.rep ~/ ".end-program-rom" ~/ tail_noise
  )
}
