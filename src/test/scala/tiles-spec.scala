package info.ditrapani.asm.tiles

import info.ditrapani.asm.Spec
import org.scalatest.EitherValues

class TilesSpec extends Spec with EitherValues {
  val ruler = "  0 1 2 3 4 5 6 7\n"
  val tile = List.fill(12)("  [][][][][][][][]").mkString("\n")

  class RowExpecter(result: Seq[Byte], start: Int) {
    def expectRow(offset: Int, value: Int): Unit = {
      result(start + offset) & 0xFF shouldBe value
    }
  }

  describe("parseStr") {
    def errorMessage(expected: String, found: String, line: Int): String =
      s"Failure([Number should be $expected, but was $found]:$line" +
        """:3 ..."\n  [][][][")"""

    it("passes if the string is well formed") {
      val stream : java.io.InputStream = getClass.getResourceAsStream("/built-in.tiles")
      val s = scala.io.Source.fromInputStream(stream).mkString
      val result: Seq[Byte] = Tiles.parseStr(s).right.value
      result.size shouldBe 1024 * 3
      // Look at 2nd tile (tile 1)
      val tile001 = new RowExpecter(result, 12 * 1)
      tile001.expectRow( 0, 0x00)     //  [][][][][][][][]  0000 0000
      tile001.expectRow( 1, 0x66)     //  []<><>[][]<><>[]  0110 0110
      tile001.expectRow( 2, 0x66)     //  []<><>[][]<><>[]  0110 0110
      tile001.expectRow( 8, 0x42)     //  []<>[][][][]<>[]  0100 0010
      tile001.expectRow( 9, 0x3C)     //  [][]<><><><>[][]  0011 1100
      tile001.expectRow(10, 0x18)     //  [][][]<><>[][][]  0001 1000
      tile001.expectRow(11, 0x00)     //  [][][][][][][][]  0000 0000
      // Look at last tile (tile 255)
      val tile255 = new RowExpecter(result, 12 * 255)
      tile255.expectRow( 0, 0x00)     //  [][][][][][][][]  0000 0000
      tile255.expectRow( 1, 0x80)     //  <>[][][][][][][]  1000 0000
      tile255.expectRow( 2, 0xC0)     //  <><>[][][][][][]  1000 0000
      tile255.expectRow( 3, 0xE0)     //  <><><>[][][][][]  1000 0000
      tile255.expectRow( 7, 0xFE)     //  <><><><><><><>[]  1111 1110
      tile255.expectRow(11, 0x00)     //  [][][][][][][][]  0000 0000
    }

    it("fails if it can't match a pixel") {
      val result = Tiles.parseStr(ruler + "00\n" + "  [>").left.value
      result shouldBe """Failure((pixel0 | pixel1):3:3 ..."[>")"""
    }

    it("fails if the first tile number is wrong") {
      val result = Tiles.parseStr(ruler + "01\n" + tile + "\n01").left.value
      result shouldBe errorMessage("00", "01", 2)
    }

    it("fails if the second tile number is wrong") {
      val result = Tiles.parseStr(ruler + "00\n" + tile + "\n00\n" + tile).left.value
      result shouldBe errorMessage("01", "00", 15)
    }
  }
}
