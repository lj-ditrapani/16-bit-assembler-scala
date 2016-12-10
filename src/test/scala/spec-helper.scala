package info.ditrapani.asm

import org.scalatest.{FunSpec, Matchers}

abstract class Spec extends FunSpec with Matchers {
  class ParserError(file_type: String) {
    def format(line: Int, column: Int, message: String): String =
      s"Failure parsing $file_type file occured at\n" +
        s"Line: $line\nColumn: $column\n$message"
  }
}
