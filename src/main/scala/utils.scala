package info.ditrapani.asm

import fastparse.all.Parsed

object Utils {
  def parsedResult2Either[T](
      file_type: String,
      result: Parsed[T]): Either[String, T] =
    result match {
      case Parsed.Success(value, index) =>
        Right(value)
      case failure: Parsed.Failure =>
        val input = failure.extra.input
        val Array(line, column) = input.repr.prettyIndex(input, failure.index).split(":")
        val s = s"Failure parsing $file_type file occured at\n" +
          s"Line: $line\nColumn: $column\n"
        Left(s + failure.msg)
    }
}
