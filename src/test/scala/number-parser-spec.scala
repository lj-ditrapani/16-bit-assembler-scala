package info.ditrapani.asm.number

import info.ditrapani.asm.Spec

class NumberParserSpec extends Spec {
  describe("number16") {
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

      val p = P(Start ~/ NumberParser.number16bit ~/ End)
      val result = p.parse(s)
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val value = result.asInstanceOf[Parsed.Success[Number16]].value
      value shouldBe Number16(v)
    }

    def failRun(s: String): Unit = {
      import fastparse.all._

      val p = P(Start ~/ NumberParser.number16bit ~/ End)
      val result = p.parse(s)
      result shouldBe a[Parsed.Failure]
    }

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
        ("32768", 0x8000),
        ("-1", 0xFFFF),
        ("-32768", 0x8000),
        ("-123", 0xFF85)
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
    }

    describe("binary numbers") {
      val bin_tests = List(
        ("%1111_1111", 255),
        ("%1010_0101", 0xA5),
        ("%0000_0000", 0x00),
        ("%1100_0110_1010_0011", 0xC6A3)
      )
      runSuccessTests("binary", bin_tests)
    }
  }
}
