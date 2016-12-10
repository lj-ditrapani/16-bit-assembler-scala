package info.ditrapani.asm.tiles

import info.ditrapani.asm
import info.ditrapani.asm.Spec

class TilesSpec extends Spec {
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
      val good = Tiles.parseStr(s)
      good shouldBe an[asm.Good]
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val result: Seq[Byte] = Tiles.parseStr(s).asInstanceOf[asm.Good].bytes
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

    it("fails if the first tile number is wrong") {
      val error = Tiles.parseStr(ruler + "01\n" + tile + "\n01")
      error shouldBe an[asm.Error]
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val result = error.asInstanceOf[asm.Error].message
      result shouldBe """Failure([Number should be 00, but was 01]:2:3 ..."\n  [][][][")"""
    }

    it("fails if the second tile number is wrong") {
      val error = Tiles.parseStr(ruler + "00\n" + tile + "\n00\n" + tile)
      error shouldBe an[asm.Error]
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val result = error.asInstanceOf[asm.Error].message
      result shouldBe """Failure([Number should be 01, but was 00]:15:3 ..."\n  [][][][")"""
    }
  }
}
