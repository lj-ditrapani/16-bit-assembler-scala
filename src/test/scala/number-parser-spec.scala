package info.ditrapani.asm

class NumberParserSpec extends Spec {
  describe("number") {
    def runTests(num_type: String, tests: Seq[(String, Int)]): Unit = {
      for(test <- tests) {
        val (string, num) = test
        ignore(s"parses ${num_type} values:  ${string} => ${num}") {
          check(string, num)
        }
      }
    }

    def check(s: String, v: Int): Unit = {
      import fastparse.all._

      val p = P(Start ~/ NumberParser.number ~/ End)
      p.parse(s) should === (v)
    }

    val decimal_tests = List(
      ("+123", 123)
    )
    runTests("decimal", decimal_tests)

    val hex_tests = List(
      ("$C6A3", 0xC6A3),
      ("$A3", 0xA3),
      ("$FFFF", 0xFFFF),
      ("$0", 0x0),
      ("$C6_A3", 0xC6A3)
    )
    runTests("hexadecimal", hex_tests)

    val bin_tests = List(
      ("%1111_1111", 255),
      ("%1010_0101", 0xA5),
      ("%0000_0000", 0x00),
      ("%1100_0110_1010_0011", 0xC6A3)
    )
    runTests("binary", bin_tests)
  }
}
