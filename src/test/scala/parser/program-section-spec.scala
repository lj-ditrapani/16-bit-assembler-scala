package info.ditrapani.asm.parser.program

import fastparse.all._
import info.ditrapani.asm.Spec
import info.ditrapani.asm.parser.number.{Number4, Number8}
import scala.collection.mutable.ArrayBuffer

class ProgramSectionSpec extends Spec {
  val parser = P(Start ~/ ProgramSection.program_section ~/ End)

  it("parses a HBY instruction") {
    val hby_instruction = ImmediateByteInstruction.hbyMaker((13, Number8(7), Number4(3)))
    parser.parse(".program-rom\nHBY 7 3\n.end-program-rom") shouldBe
      Parsed.Success(ArrayBuffer(hby_instruction), 37)
  }

  it("parses an adding program") {
    val program = """.program-rom
      |# Adds two number togeth
      |# RA (register 10) is used for all addresses
      |# A is stored in ram[$0100]
      |# B is stored in ram[$0101]
      |# Add A and B and store in ram[$0102]
      |# Put A in R1
      |# Put B in R2
      |# Add A + B and put in R3
      |# Store R3 into ram[0102]
      |
      |# Set RA to $100
      |HBY $01 10
      |LBY $00 10
      |
      |LOD 10 1     # Load value at ram[RA] => R1
      |LBY $01 10   # Set RA to $101
      |LOD 10 2     # Load value at ram[RA] => R2
      |ADD 1 2 3
      |LBY $02 10   # Set RA to $102
      |STR 10 3     # Store value in R3 into ram[RA]
      |END
      |
      |.end-program-rom""".stripMargin
    parser.parse(program).get.value shouldBe
      ArrayBuffer[RealInstruction](
        ImmediateByteInstruction.hbyMaker((275, Number8(1), Number4(10))),
        ImmediateByteInstruction.lbyMaker((286, Number8(0), Number4(10))),
        TwoOperandInstruction("LOD", 0x30, 298, Number4(10), Number4(1)),
        ImmediateByteInstruction.lbyMaker((341, Number8(1), Number4(10))),
        TwoOperandInstruction("LOD", 0x30, 371, Number4(10), Number4(2)),
        ThreeOperandInstruction("ADD", 0x50, 414, Number4(1), Number4(2), Number4(3)),
        ImmediateByteInstruction.lbyMaker((424, Number8(2), Number4(10))),
        Str(454, Number4(10), Number4(3)),
        IEnd(500)
      )
  }
}
