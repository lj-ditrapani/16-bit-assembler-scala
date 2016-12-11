package info.ditrapani.asm

import scala.collection.mutable.ArrayBuffer
import info.ditrapani.asm.SymbolsSection.SymbolEntry
import fastparse.all._

class SymbolsSectionSpec extends Spec {
  describe("symbols_section") {

    val parser = P(Start ~/ SymbolsSection.symbols_section ~/ End)

    it("can be empty") {
      val result = parser.parse(".symbols\n.end-symbols")
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val value = result.asInstanceOf[Parsed.Success[ArrayBuffer[SymbolEntry]]].value
      value shouldBe ArrayBuffer()
    }

    it("succeeds even if it does not contain any SymbolEntrys") {
      val result = parser.parse(
        """.symbols
          |# A comment
          |
          |  
          |# Empty lines & lines with spaces
          |.end-symbols""".stripMargin)
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val value = result.asInstanceOf[Parsed.Success[ArrayBuffer[SymbolEntry]]].value
      value shouldBe ArrayBuffer()
    }

    ignore("parses and returns SymbolEntrys") {
      val result = parser.parse(
        """.symbols
          |
          |# A comment
          |size 16
          |secret-to-life 42
          |
          |  
          |# Empty lines & lines with spaces
          |LOOP %1111_0000_1000_0111
          |MY_VAR $0F0F
          |neg_ -123
          |   # Comment
          |
          |  
          |.end-symbols""".stripMargin)
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val value = result.asInstanceOf[Parsed.Success[ArrayBuffer[SymbolEntry]]].value
      value shouldBe ArrayBuffer()
    }
  }
}
