package info.ditrapani.asm

sealed abstract class Result

final case class Good(binary_file: Seq[Byte]) extends Result
final case class Error(message: String) extends Result
object Help extends Result
