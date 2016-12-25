package info.ditrapani.asm

import org.scalatest.EitherValues
import scala.collection.mutable.ArrayBuffer

class MainSpec extends Spec with EitherValues {
  describe("process") {
    def expectError(result: Result, message: String): Unit = {
      result shouldBe an[Error]
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val value = result.asInstanceOf[Error].message
      value shouldBe message
    }

    def expectParseError(
        result: Result,
        line: Int,
        column: Int,
        error_message: String): Unit = {
      val message = "Failure parsing ASCII tile file occured at\n" +
        s"Line: $line\nColumn: $column\n$error_message"
      expectError(result, message)
    }

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def expectGood(result: Result): Seq[Byte] = {
      result shouldBe a[Good]
      result.asInstanceOf[Good].bytes
    }

    describe("with 0 args") {
      it("prints an error") {
        val args = Array[String]()
        val result = Main.process(args)
        val message = "Missing command line arguments; requires 1 or 2"
        expectError(result, message)
      }
    }

    describe("with 1 arg") {
      describe("arg is -h") {
        it("prints the help text") {
          val args = Array("-h")
          val result = Main.process(args)
          result shouldBe Help
        }
      }

      describe("arg is --help") {
        it("prints the help text") {
          val args = Array("--help")
          val result = Main.process(args)
          result shouldBe Help
        }
      }

      describe("arg is a file") {
        it("assembles the file into a binary") {
          val good_file = "src/test/resources/super-basic-test.asm"
          val args = Array(good_file)
          val result = Main.process(args)
          result shouldBe Good(ArrayBuffer(
            0x10, 0x1A,     // HBY $01 RA
            0x20, 0x0A,     // LBY $00 RA
            0x3A, 0x01,     // LOD RA R1      ram[RA] => R1
            0x20, 0x1A,     // LBY $01 RA
            0x3A, 0x02,     // LOD RA R2      ram[RA] => R2
            0x51, 0x23,     // ADD R1 R2 R3   R1 + R2 => R3
            0x20, 0x2A,     // LBY $00 RA
            0x4A, 0x30,     // STR RA R3      R3 => ram[RA]
            0x00, 0x00      // END
          ).map(_.toByte))
        }
      }

      describe("arg is neither a file nor -h nor --help") {
        it("prints an error") {
          val args = Array("--one")
          val result = Main.process(args)
          val message = "java.io.FileNotFoundException: --one " +
            "(No such file or directory)"
          expectError(result, message)
        }
      }
    }

    describe("with 2 args") {
      val good_file = "src/test/resources/built-in.tiles"

      def checkTiles(result: Result): Unit = {
        val value = expectGood(result)
        value.length shouldBe 3072
        value(12 + 1) & 0xFF shouldBe 0x66   //  []<><>[][]<><>[]  0110 0110
      }

      describe("first arg = -t") {
        describe("second arg is a good ascii tile file") {
          it("transforms a good ascii tile file into a binary one") {
            val args = Array("-t", good_file)
            val result = Main.process(args)
            checkTiles(result)
          }
        }

        describe("second arg is a file, but not a good ascii tile file") {
          it("prints an error") {
            val args = Array("-t", "README.md")
            val result = Main.process(args)
            val message = """"  0 1 2 3 4 5 6 7\n":1:1 ..."Assembler """"
            expectParseError(result, 1, 1, message)
          }
        }

        describe("second arg is not a file") {
          it("prints an error") {
            val args = Array("-t", "not_a_file")
            val result = Main.process(args)
            val message = "java.io.FileNotFoundException: not_a_file " +
              "(No such file or directory)"
            expectError(result, message)
          }
        }
      }

      describe("first arg = --tiles") {
        it("transforms a good ascii tile file into a binary one") {
          val args = Array("-t", good_file)
          val result = Main.process(args)
          checkTiles(result)
        }
      }

      describe("first arg is neither -t nor --tiles") {
        it("prints an error") {
          val args = Array("--one", "file")
          val result = Main.process(args)
          val message = "The two argument form is to create binary tile sets, " +
            "the first argument must be -t or --tiles."
          expectError(result, message)
        }
      }
    }

    describe("with greater than 2 args") {
      it("prints an error") {
        val args = Array("--one", "--two", "--three")
        val result = Main.process(args)
        val message = "Too many command line arguments; supply 1 or 2"
        expectError(result, message)
      }
    }
  }
}
