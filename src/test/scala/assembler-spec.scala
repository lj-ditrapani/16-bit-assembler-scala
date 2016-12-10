package info.ditrapani.asm

class AssemblerSpec extends Spec {
  val parseErrorMessage = (new ParserError("assembly")).format _

  it("returns a Left if it contains control characters other than Line Feed ($0A \\n)") {
    val expected_message = parseErrorMessage(1, 1, "End:1:1 ...\"\\u0000\"")
    Assembler.assemble("\u0000") shouldBe Left(expected_message)
  }
}
