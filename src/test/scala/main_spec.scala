package info.ditrapani.asm

import org.scalatest.EitherValues

class MainSpec extends Spec with EitherValues {
  describe("process") {
    def expectError(args: Array[String], message: String): Unit = {
    }

    describe("with 0 args") {
      it("prints an error") {
        val args = Array[String]()
        val result = Main.process(args)
        result shouldBe an[Error]
        @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
        val value = result.asInstanceOf[Error].message
        value shouldBe "Missing command line arguments; requires 1 or 2"
      }
    }

    describe("with 1 arg") {
      describe("arg is -h") {
        val args = Array("-h")
        val result = Main.process(args)
        result shouldBe Help
      }

      describe("arg is --help") {
        val args = Array("--help")
        val result = Main.process(args)
        result shouldBe Help
      }

      describe("arg is neither -h nor --help") {
        it("raises no such file exception") {
          val args = Array("--one")
          val result = Main.process(args)
          result shouldBe an[Error]
          @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
          val value = result.asInstanceOf[Error].message
          value shouldBe "java.io.FileNotFoundException: --one " +
            "(No such file or directory)"
        }
      }
    }

    describe("with 2 args") {
      describe("first arg = -t") {
        it("transforms a good ascii tile file into a binary one") {
          val args = Array("-t", "src/test/resources/built-in.tiles")
          val result = Main.process(args)
          result shouldBe a[Good]
          @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
          val value = result.asInstanceOf[Good].bytes
          value.length shouldBe 3072
          value(12 + 1) & 0xFF shouldBe 0x66   //  []<><>[][]<><>[]  0110 0110
        }
      }

      describe("first arg = --tiles") {
        it("transforms a good ascii tile file into a binary one") {
          val args = Array("-t", "src/test/resources/built-in.tiles")
          val result = Main.process(args)
          result shouldBe a[Good]
          @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
          val value = result.asInstanceOf[Good].bytes
          value.length shouldBe 3072
          value(12 + 1) & 0xFF shouldBe 0x66   //  []<><>[][]<><>[]  0110 0110
        }
      }

      describe("first arg = --tiles") {
        it("first arg is neither -t nor --tiles") {
          val args = Array("--one", "file")
          val result = Main.process(args)
          result shouldBe an[Error]
          @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
          val value = result.asInstanceOf[Error].message
          value shouldBe "The two argument form is to create binary tile sets, " +
            "the first argument must be -t or --tiles."
        }
      }
    }

    describe("with greater than 2 args") {
      it("prints an error") {
        val args = Array("--one", "--two", "--three")
        val result = Main.process(args)
        result shouldBe an[Error]
        @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
        val value = result.asInstanceOf[Error].message
        value shouldBe "Too many command line arguments; supply 1 or 2"
      }
    }
  }
}
