package info.ditrapani.asm.symboltable

import info.ditrapani.asm.Spec

class SymbolTableSpec extends Spec {
  describe("predefined_symbols") {
    it("is defined") {
      import SymbolTable.predefined_symbols
      predefined_symbols.size shouldBe 16 + 6 + 5
      predefined_symbols("R0").value shouldBe 0
      predefined_symbols("R15").value shouldBe 15
      predefined_symbols("RA").value shouldBe 10
      predefined_symbols("RF").value shouldBe 15
      predefined_symbols("gamepad").value shouldBe 0xFFFD
    }
  }
}
