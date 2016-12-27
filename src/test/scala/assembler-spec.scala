package info.ditrapani.asm

import org.scalatest.EitherValues

class AssemblerSpec extends Spec with EitherValues {
  val parseErrorMessage = (new ParserError("assembly")).format _

  it("returns a Left if it contains control characters other than Line Feed ($0A \\n)") {
    val expected_message = parseErrorMessage(1, 1, "End:1:1 ...\"\\u0000\"")
    Assembler.assemble("\u0000") shouldBe Left(expected_message)
  }

  describe("when provided full, correct assembly programs") {
    it("assembles a branching program") {
      val program = """
        |.symbols
        |.end-symbols
        |.program-rom
        |# RA (register 10) is used for all value addresses
        |# RB has address of 2nd branch
        |# RC has address of final, common, end of program
        |# A is stored in ram[0100]
        |# B is stored in ram[0101]
        |# If A - B < 3, store 255 in ram[0102], else store 1 in ram[0102]
        |# Put A in R1
        |# Put B in R2
        |# Sub A - B and put in R3
        |# Load const 3 into R4
        |# Sub R3 - R4 => R5
        |# If R5 is negative, 255 => R6, else 1 => R6
        |# Store R6 into ram[0102]
        |# Load 2nd branch address into RB
        |HBY $00 $B
        |LBY $10 $B

        |# Load end of program address int RC
        |ADI $B 2 $C

        |# Load A value into R1
        |HBY $01 $A
        |LBY $00 $A
        |LOD $A $1

        |# Load B value into R2
        |LBY $01 $A
        |LOD $A $2

        |SUB $1 $2 $3

        |# Load constant 3 to R4
        |HBY $00 $4
        |LBY $03 $4

        |SUB $3 $4 $5

        |# Branch to ? if A - B >= 3
        |BRV $5 ZP $B

        |# Load constant 255 into R6
        |HBY $00 $6
        |LBY $FF $6
        |BRV $0 NZP $C      # (Jump to end)

        |# Load constant $01 into R6
        |HBY $00 $6
        |LBY $01 $6

        |# Store final value into ram[0102]
        |LBY $02 $A
        |STR $A $6
        |END
        |.end-program-rom
        |.video-rom
        |.end-video-rom
        |.data-ram
        |.end-data-ram
      """.stripMargin

      Assembler.assemble(program).right.value shouldBe Seq(
        // Load 2nd branch address into RB
        0x10, 0x0B,    // 00 HBY 0x00 RB
        0x21, 0x0B,    // 01 LBY 0x10 RB

        // Load end of program address int RC
        0x7B, 0x2C,    // 02 ADI RB 2 RC

        // Load A value into R1
        0x10, 0x1A,    // 03 HBY 0x01 RA
        0x20, 0x0A,    // 04 LBY 0x00 RA
        0x3A, 0x01,    // 05 LOD RA R1

        // Load B value into R2
        0x20, 0x1A,    // 06 LBY 0x01 RA
        0x3A, 0x02,    // 07 LOD RA R2

        0x61, 0x23,    // 08 SUB R1 R2 R3

        // Load constant 3 to R4
        0x10, 0x04,    // 09 HBY 0x00 R4
        0x20, 0x34,    // 0A LBY 0x03 R4

        0x63, 0x45,    // 0B SUB R3 R4 R5

        // Branch to ? if A - B >= 3
        0xE5, 0xB3,    // 0C BRV R5 RB ZP

        // Load constant 255 into R6
        0x10, 0x06,    // 0D HBY 0x00 R6
        0x2F, 0xF6,    // 0E LBY 0xFF R6
        0xE0, 0xC7,    // 0F BRV R0 RC NZP (Jump to end)

        // Load constant 0x01 into R6
        0x10, 0x06,    // 10 HBY 0x00 R6
        0x20, 0x16,    // 11 LBY 0x01 R6

        // Store final value into ram[0102]
        0x20, 0x2A,    // 12 LBY 0x02 RA
        0x4A, 0x60,    // 13 STR RA R6
        0x00, 0x00     // 14 END
      ).map(_.toByte)
    }

    it("assembles a program with a while loop") {
      val program = """
        |.symbols
        |.end-symbols
        |.program-rom
        |# Input: n followed by a list of n integers
        |# Output: -2 * sum(list of n integers)

        |# R0 gets address of beginning of input from storage space
        | HBY $E8 $0        # $E8 -> Upper(R0)
        | LBY $00 $0        # $00 -> Lower(R0)

        |# R1 gets address of begining of output to storage space
        | HBY $EC $1        # $EC -> Upper(R1)
        | LBY $00 $1        # $00 -> Lower(R1)

        |# R2 gets n, the count of how many input values to sum
        | LOD $0 $2         # First Input (count n) -> R2

        |# R3 and R4 have start and end of while loop respectively
        | LBY $07 $3        # addr start of while loop -> R3
        | LBY $0D $4        # addr to end while loop -> R4

        |# Start of while loop
        | BRV $2 Z $4       # if R2 is zero ($.... -> PC)
        | ADI $0 1 $0       # increment input address
        | LOD $0 $6         # Next Input -> R6
        | ADD $5 $6 $5      # R5 + R6 (running sum) -> R5
        | SBI $2 1 $2       # R2 - 1 -> R2
        | BRV $0 NZP $3     # $.... -> PC (unconditional)

        |# End of while loop
        | SHF $5 L 1 $6     # Double sum

        |# Negate double of sum
        | SUB $7 $6 $7      # 0 - R6 -> R7

        |# Output result
        | STR $1 $7         # Output value of R7
        | END
        |.end-program-rom
        |.video-rom
        |.end-video-rom
        |.data-ram
        |.end-data-ram
      """.stripMargin

      Assembler.assemble(program).right.value shouldBe Seq(
        // R0 gets address of beginning of input from storage space
        0x1E, 0x80,      // 0 HBY 0xE8 R0       0xE8 -> Upper(R0)
        0x20, 0x00,      // 1 LBY 0x00 R0       0x00 -> Lower(R0)

        // R1 gets address of begining of output to storage space
        0x1E, 0xC1,      // 2 HBY 0xEC R1       0xEC -> Upper(R1)
        0x20, 0x01,      // 3 LBY 0x00 R1       0x00 -> Lower(R1)

        // R2 gets n, the count of how many input values to sum
        0x30, 0x02,      // 4 LOD R0 R2         First Input (count n) -> R2

        // R3 and R4 have start and end of while loop respectively
        0x20, 0x73,      // 5 LBY 0x07 R3       addr start of while loop -> R3
        0x20, 0xD4,      // 6 LBY 0x0D R4       addr to end while loop -> R4

        // Start of while loop
        0xE2, 0x42,      // 7 BRV R2 R4 Z       if R2 is zero (0x.... -> PC)
        0x70, 0x10,      // 8 ADI R0 1 R0       increment input address
        0x30, 0x06,      // 9 LOD R0 R6         Next Input -> R6
        0x55, 0x65,      // A ADD R5 R6 R5      R5 + R6 (running sum) -> R5
        0x82, 0x12,      // B SBI R2 1 R2       R2 - 1 -> R2
        0xE0, 0x37,      // C BRV R0 R3 NZP     0x.... -> PC (unconditional)

        // End of while loop
        0xD5, 0x06,      // D SHF R5 left 1 R6  Double sum

        // Negate double of sum
        0x67, 0x67,      // E SUB R7 R6 R7      0 - R6 -> R7

        // Output result
        0x41, 0x70,      // F STR R1 R7         Output value of R7
        0x00, 0x00       //   END
      ).map(_.toByte)
    }

    it("assembles a program that adds/subtracs/shifts with carries & overflows") {
      val program = """
        |.symbols
        |.end-symbols
        |.program-rom
        |# load word $4005 in to R0
        |# shf left 2 (causes carry to be set)
        |# store result in $0000 (should get $0014)
        |# BRF C (branch if carry set)
        |# END   (gets skipped over)
        |# 32766 + 1
        |# BRF V to END (does not take branch)
        |# 32767 + 1
        |# BRF V
        |# END   (gets skipped over)
        |# 65534 + 1
        |# BRF C to END (does not take branch)
        |# 65535 + 1
        |# BRF C
        |# END   (gets skipped over)
        |# store $FACE in $0001

        |# Shift & branch on carry
        | HBY $40 $0        # $40 -> Upper(R0)
        | LBY $05 $0        # $05 -> Lower(R0)
        | SHF $0 L 2 $0     # Shift R0 Left by 2 -> R0
        | HBY $00 $A
        | LBY $00 $A
        | STR $A $0         # shifted value -> M[$0000]
        | LBY $09 $A        # RA = $0009
        | BRF C $A          # Jump to $0009 if carry set
        | END               # Gets skipped

        |# Add & branch on overflow
        |# R0 = $7FFE
        | HBY $7F $0        # $7F -> Upper(R0)
        | LBY $FE $0        # $FE -> Lower(R0)
        | ADI $0 1 $0       # R0 = $7FFE + 1
        | LBY $08 $A        # RA = $0008
        | BRF V $A          # Do not jump, overflow not set
        | ADI $0 1 $0       # R0 = $7FFF + 1
        | LBY $12 $A        # RA = $0012
        | BRF V $A          # Jump
        | END               # Gets skipped

        |# Add & branch on carry
        |# R0 = $FFFE
        | HBY $FF $0        # $FF -> Upper(R0)
        | LBY $FE $0        # $FE -> Lower(R0)
        | ADI $0 1 $0       # R0 = $FFFE + 1
        | LBY $08 $A        # RA = $0008
        | BRF C $A          # Jump to $0008 if carry set
        | ADI $0 1 $0       # R0 = $FFFF + 1
        | LBY $1B $A        # RA = $001B
        | BRF C $A          # Jump to $001B if carry set
        | END               # Gets skipped
        |# R0 = $FACE
        | HBY $FA $0
        | LBY $CE $0
        | LBY $01 $A        # RA = $0001
        | STR $A $0         # STR R0 -> M[RA]   # $FACE -> M[$0001]
        | END
        |.end-program-rom
        |.video-rom
        |.end-video-rom
        |.data-ram
        |.end-data-ram
      """.stripMargin

      Assembler.assemble(program).right.value shouldBe Seq(
        // Shift & branch on carry
        0x14, 0x00,     // 00 HBY 0x40 R0       0x40 -> Upper(R0)
        0x20, 0x50,     // 01 LBY 0x05 R0       0x05 -> Lower(R0)
        0xD0, 0x10,     // 02 SHF R0 Left by 2 -> R0
        0x10, 0x0A,     // 03 HBY 0x00 RA
        0x20, 0x0A,     // 04 LBY 0x00 RA
        0x4A, 0x00,     // 05 STR R0 -> M[RA]   shifted value -> M[$0000]
        0x20, 0x9A,     // 06 LBY 0x09 RA       RA = 0x0009
        0xF0, 0xA1,     // 07 BRF RA C          Jump to 0x0009 if carry set
        0x00, 0x00,     // 08 END               Gets skipped

        // Add & branch on overflow
        // R0 = 0x7FFE
        0x17, 0xF0,     // 09 HBY 0x7F R0       0x7F -> Upper(R0)
        0x2F, 0xE0,     // 0A LBY 0xFE R0       0xFE -> Lower(R0)
        0x70, 0x10,     // 0B ADI R0 1 R0       R0 = 0x7FFE + 1
        0x20, 0x8A,     // 0C LBY 0x08 RA       RA = 0x0008
        0xF0, 0xA2,     // 0D BRF RA V          Do not jump, overflow not set
        0x70, 0x10,     // 0E ADI R0 1 R0       R0 = 0x7FFF + 1
        0x21, 0x2A,     // 0F LBY 0x12 RA       RA = 0x0012
        0xF0, 0xA2,     // 10 BRF RA V          Jump
        0x00, 0x00,     // 11 END               Gets skipped

        // Add & branch on carry
        // R0 = 0xFFFE
        0x1F, 0xF0,     // 12 HBY 0xFF R0       0xFF -> Upper(R0)
        0x2F, 0xE0,     // 13 LBY 0xFE R0       0xFE -> Lower(R0)
        0x70, 0x10,     // 14 ADI R0 1 R0       R0 = 0xFFFE + 1
        0x20, 0x8A,     // 15 LBY 0x08 RA       RA = 0x0008
        0xF0, 0xA1,     // 16 BRF RA C          Jump to 0x0008 if carry set
        0x70, 0x10,     // 17 ADI R0 1 R0       R0 = 0xFFFF + 1
        0x21, 0xBA,     // 18 LBY 0x1B RA       RA = 0x001B
        0xF0, 0xA1,     // 19 BRF RA C          Jump to 0x001B if carry set
        0x00, 0x00,     // 1A END               Gets skipped
        // R0 = 0xFACE
        0x1F, 0xA0,     // 1B HBY 0xFA R0
        0x2C, 0xE0,     // 1C LBY 0xCE R0
        0x20, 0x1A,     // 1D LBY 0x01 RA       RA = 0x0001
        0x4A, 0x00,     // 1E STR R0 -> M[RA]   0xFACE -> M[$0001]
        0x00, 0x00      // 1F END
      ).map(_.toByte)
    }
  }
}
