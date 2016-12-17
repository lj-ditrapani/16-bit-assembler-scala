package info.ditrapani.asm.parser.symbols

import scala.collection.mutable.ArrayBuffer
import info.ditrapani.asm.parser.number.Number16
import info.ditrapani.asm.Spec
import SymbolsSection.SymbolEntry
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
      // scalastyle:off whitespace.end.of.line
      val result = parser.parse(
        """.symbols
          |# A comment
          |
          |  
          |# Empty lines & lines with spaces
          |.end-symbols""".stripMargin)
      // scalastyle:on whitespace.end.of.line
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val value = result.asInstanceOf[Parsed.Success[ArrayBuffer[SymbolEntry]]].value
      value shouldBe ArrayBuffer()
    }

    it("parses and returns SymbolEntrys") {
      // scalastyle:off whitespace.end.of.line
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
      // scalastyle:on whitespace.end.of.line
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val value = result.asInstanceOf[Parsed.Success[ArrayBuffer[SymbolEntry]]].value
      val entries = List(
        (22, "size", 16),
        (30, "secret-to-life", 42),
        (86, "LOOP", 0xF087),
        (112, "MY_VAR", 0x0F0F),
        (125, "neg_", 0xFF85)
      ).map {
        case (index, name, value) => SymbolEntry(index, name, Number16(value))
      }
      value shouldBe entries.to[ArrayBuffer]
    }
  }
}
