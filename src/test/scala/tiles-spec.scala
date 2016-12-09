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
    it("passes if the string is well formed") {
      val stream : java.io.InputStream = getClass.getResourceAsStream("/built-in.tiles")
      val s = scala.io.Source.fromInputStream(stream).mkString
      val result: Seq[Byte] = Tiles.parseStr(s).right.value
      result.size shouldBe 1024 * 3
      // Look at 2nd tile (tile 1), rows 0-2 & 8-10
      result(12 +  0) shouldBe 0x00   //  [][][][][][][][]  0000 0000
      result(12 +  1) shouldBe 0x66   //  []<><>[][]<><>[]  0110 0110
      result(12 +  2) shouldBe 0x66   //  []<><>[][]<><>[]  0110 0110
      result(12 +  8) shouldBe 0x42   //  []<>[][][][]<>[]  0100 0010
      result(12 +  9) shouldBe 0x3C   //  [][]<><><><>[][]  0011 1100
      result(12 + 10) shouldBe 0x18   //  [][][]<><>[][][]  0001 1000
      // Look at last tile
      result(3060 +  0) & 0xFF shouldBe 0x00  //  [][][][][][][][]  0000 0000
      result(3060 +  1) & 0xFF shouldBe 0x80  //  <>[][][][][][][]  1000 0000
      result(3060 +  2) & 0xFF shouldBe 0xC0  //  <><>[][][][][][]  1000 0000
      result(3060 +  3) & 0xFF shouldBe 0xE0  //  <><><>[][][][][]  1000 0000
      result(3060 + 11) & 0xFF shouldBe 0x00  //  [][][][][][][][]  0000 0000
    }

    it("fails if the first tile number is wrong") {
      val result = Tiles.parseStr(ruler + "01\n" + tile + "\n01").left.value
      result shouldBe """Failure(Fail:2:3 ..."\n  [][][][")"""
    }

    it("fails if the second tile number is wrong") {
      val result = Tiles.parseStr(ruler + "00\n" + tile + "\n02\n" + tile).left.value
      result shouldBe """Failure(Fail:15:3 ..."\n  [][][][")"""
    }
  }
}
