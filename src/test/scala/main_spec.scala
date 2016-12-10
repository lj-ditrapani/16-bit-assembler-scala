package info.ditrapani.asm

import org.scalatest.EitherValues

class MainSpec extends Spec with EitherValues {
  describe("process") {
    describe("with 0 args") {
    }

    describe("-t") {
      it("transforms a good ascii tile file into a binary one") {
        val args = Array("-t", "src/test/resources/built-in.tiles")
        val right = Main.process(args)
        right shouldBe a[Good]
        @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
        val result = right.asInstanceOf[Good].bytes
        result.length shouldBe 3072
        result(12 + 1) & 0xFF shouldBe 0x66   //  []<><>[][]<><>[]  0110 0110
      }
    }
  }
}
