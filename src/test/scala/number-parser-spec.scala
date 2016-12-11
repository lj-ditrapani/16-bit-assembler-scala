package info.ditrapani.asm.number

import info.ditrapani.asm.Spec

class NumberParserSpec extends Spec {
  trait Fixture[T] {
    import fastparse.all._
    val number_parser: Parser[T]

    def makeNumber(value: Int): T

    def runSuccessTests(num_type: String, tests: List[(String, Int)]): Unit = {
      for(test <- tests) {
        val (string, num) = test
        it(s"parses ${num_type} values:  ${string} => ${num}") {
          check(string, num)
        }
      }
    }

    def runFailTests(num_type: String, tests: List[String]): Unit = {
      for(string <- tests) {
        it(s"fails on ${num_type} value:  ${string}") {
          failRun(string)
        }
      }
    }

    def check(s: String, v: Int): Unit = {
      import fastparse.all._

      val p = P(Start ~/ number_parser ~/ End)
      val result = p.parse(s)
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val value = result.asInstanceOf[Parsed.Success[T]].value
      value shouldBe makeNumber(v)
    }

    def failRun(s: String): Unit = {
      import fastparse.all._

      val p = P(Start ~/ number_parser ~/ End)
      val result = p.parse(s)
      result shouldBe a[Parsed.Failure]
    }
  }

  trait Number16Fixture extends Fixture[Number16] {
    val number_parser = NumberParser.number16bit
    def makeNumber(value: Int) = Number16.apply(value)
  }

  trait Number8Fixture extends Fixture[Number8] {
    val number_parser = NumberParser.number8bit
    def makeNumber(value: Int) = Number8.apply(value)
  }

  trait Number4Fixture extends Fixture[Number4] {
    val number_parser = NumberParser.number4bit
    def makeNumber(value: Int) = Number4.apply(value)
  }

  describe("16-bit numbers") { new Number16Fixture {
    describe("decimal numbers") {
      val decimal_pass_tests = List(
        ("0", 0),
        ("+0", 0),
        ("-0", 0),
        ("-0_", 0),
        ("-0__", 0),
        ("+123", 123),
        ("123", 123),
        ("10_000", 10000),
        ("65535", 0xFFFF),
        ("32767", 0x7FFF),
        ("32768", 0x8000),
        ("-1", 0xFFFF),
        ("-32768", 0x8000),
        ("-123", 0xFF85),
        ("007", 7),
        ("+007", 7),
        ("-007", 0xFFF9)
      )
      runSuccessTests("decimal", decimal_pass_tests)

      val decimal_fail_tests = List(
        "65536",
        "-32769",
        "_5",
        "5A"
      )
      runFailTests("decimal", decimal_fail_tests)
    }

    describe("hexadecimal numbers") {
      val hex_tests = List(
        ("$C6A3", 0xC6A3),
        ("$A3", 0xA3),
        ("$FFFF", 0xFFFF),
        ("$0", 0x0),
        ("$C6_A3", 0xC6A3)
      )
      runSuccessTests("hexadecimal", hex_tests)

      val hex_fail_tests = List(
        "$10000",
        "$1_0000",
        "$_5",
        "$5G"
      )
      runFailTests("hexadecimal", hex_fail_tests)
    }

    describe("binary numbers") {
      val bin_tests = List(
        ("%0", 0),
        ("%1", 1),
        ("%10", 2),
        ("%1111_1111", 255),
        ("%1010_0101", 0xA5),
        ("%0000_0000", 0x00),
        ("%1100_0110_1010_0011", 0xC6A3),
        ("%0000_0000_0000_0000", 0),
        ("%1111_1111_1111_1111", 0xFFFF)
      )
      runSuccessTests("binary", bin_tests)

      val bin_fail_tests = List(
        "%1_0000_0000_0000_0000",
        "%12",
        "%1A"
      )
      runFailTests("binary", bin_fail_tests)
    }
  }}

  describe("8-bit numbers") { new Number8Fixture {
    describe("decimal numbers") {
      val decimal_pass_tests = List(
        ("0", 0),
        ("+0", 0),
        ("-0", 0),
        ("+123", 123),
        ("123", 123),
        ("2_5_5", 0xFF),
        ("127", 0x7F),
        ("128", 0x80),
        ("-1", 0xFF),
        ("-128", 0x80),
        ("-123", 0x85),
        ("007", 7),
        ("+007", 7),
        ("-007", 0xF9)
      )
      runSuccessTests("decimal", decimal_pass_tests)

      val decimal_fail_tests = List(
        "256",
        "-129",
        "_5",
        "5A"
      )
      runFailTests("decimal", decimal_fail_tests)
    }

    describe("hexadecimal numbers") {
      val hex_tests = List(
        ("$C6", 0xC6),
        ("$A", 0xA),
        ("$FF", 0xFF),
        ("$0", 0x0),
        ("$C_6", 0xC6)
      )
      runSuccessTests("hexadecimal", hex_tests)

      val hex_fail_tests = List(
        "$100",
        "$1_0_0",
        "$_5",
        "$5G"
      )
      runFailTests("hexadecimal", hex_fail_tests)
    }

    describe("binary numbers") {
      val bin_tests = List(
        ("%0", 0),
        ("%1", 1),
        ("%10", 2),
        ("%1010_0101", 0xA5),
        ("%0000_0000", 0x00),
        ("%1100_0110", 0xC6),
        ("%0000_0000_0000_0000", 0),
        ("%1111_1111", 0xFF)
      )
      runSuccessTests("binary", bin_tests)

      val bin_fail_tests = List(
        "%1_0000_0000",
        "%12",
        "%1A"
      )
      runFailTests("binary", bin_fail_tests)
    }
  }}

  describe("4-bit numbers") { new Number4Fixture {
    describe("decimal numbers") {
      val decimal_pass_tests = List(
        ("0", 0),
        ("+0", 0),
        ("-0", 0),
        ("+11", 11),
        ("11", 11),
        ("1_0", 10),
        ("15", 0xF),
        ("8", 0x8),
        ("-1", 0xF),
        ("-8", 0x8),
        ("007", 7),
        ("+007", 7),
        ("-007", 0x9)
      )
      runSuccessTests("decimal", decimal_pass_tests)

      val decimal_fail_tests = List(
        "16",
        "-9",
        "_5",
        "A"
      )
      runFailTests("decimal", decimal_fail_tests)
    }

    describe("hexadecimal numbers") {
      val hex_tests = List(
        ("$D", 0xD),
        ("$A", 0xA),
        ("$F", 0xF),
        ("$0", 0x0),
        ("$3", 0x3)
      )
      runSuccessTests("hexadecimal", hex_tests)

      val hex_fail_tests = List(
        "$10",
        "$1_0",
        "$_5",
        "$G"
      )
      runFailTests("hexadecimal", hex_fail_tests)
    }

    describe("binary numbers") {
      val bin_tests = List(
        ("%0", 0),
        ("%1", 1),
        ("%10", 2),
        ("%1111", 0xF),
        ("%1010", 0xA),
        ("%0000_0000", 0x00),
        ("%11_00", 0xC),
        ("%0000_0000_0000_0000", 0),
        ("%1_11_1", 0xF)
      )
      runSuccessTests("binary", bin_tests)

      val bin_fail_tests = List(
        "%1_0000",
        "%2",
        "%A"
      )
      runFailTests("binary", bin_fail_tests)
    }
  }}
}
