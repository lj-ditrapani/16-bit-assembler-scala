package info.ditrapani.asm.parser.number

import fastparse.all._

sealed abstract class Sign
final object Plus extends Sign
final object Minus extends Sign
object Sign {
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def fromOption(o: Option[String]): Sign = o match {
    case None => Plus
    case Some(s) => s match {
      case "+" => Plus
      case "-" => Minus
      case x => throw new RuntimeException(s"Invalid Sign input ${x}")
    }
  }
}

final case class SignedNumber(sign: Sign, value: Int)

object SignedNumber {
  def fromHex(tuple: (String, String)): SignedNumber = make(tuple, 16)

  def fromDecimal(tuple: (Option[String], String, String)): SignedNumber = {
    val (sign_option, first_digit, digits) = tuple
    make2(Sign.fromOption(sign_option), first_digit, digits, 10)
  }

  def fromBinary(tuple: (String, String)): SignedNumber = make(tuple, 2)

  private def make(tuple: (String, String), base: Int) = {
    val (first_digit, digits) = tuple
    make2(Plus, first_digit, digits, base)
  }

  private def make2(sign: Sign, first_digit: String, digits: String, base: Int) = {
    val clean = first_digit + digits.filter(_ != '_')
    SignedNumber(sign, Integer.parseInt(clean, base))
  }
}

final case class Number16(value: Int)
final case class Number8(value: Int)
final case class Number4(value: Int)

sealed abstract class FromSignedNumber[T](max_positive: Int, max_negative: Int) {
  def apply(n: Int): T

  def fromSignedNumber(number: SignedNumber): Parser[T] =
    number.sign match {
      case Plus => (number.value > max_positive) match {
        case false => Pass.map(_ => apply(number.value))
        case true => Fail.opaque(
          s"Positive number is too large; max is ${max_positive}"
        )
      }
      case Minus => (number.value > max_negative) match {
        case false => Pass.map(_ => apply((~number.value + 1) & max_positive))
        case true => Fail.opaque(
          s"Negative number is too large; max is -${max_negative}"
        )
      }
    }
}

object Number16 extends FromSignedNumber[Number16](0xFFFF, 0x8000)

object Number8 extends FromSignedNumber[Number8](0xFF, 0x80)

object Number4 extends FromSignedNumber[Number4](0xF, 0x8)

object NumberParser {
  private val decimal_digit = P(CharIn('0' to '9'))
  private val hex_letters = P(CharIn('a' to 'f') | CharIn('A' to 'F'))
  private val hex_digit = P(decimal_digit | hex_letters)
  private val binary_digit = P(CharIn("01"))
  private val decimal = (("+" | "-").!.? ~ decimal_digit.! ~/ (decimal_digit | "_").rep.!)
    .map(SignedNumber.fromDecimal)
  private val hex = P("$" ~/ hex_digit.! ~/ (hex_digit | "_").rep.!)
    .map(SignedNumber.fromHex)
  private val binary = P("%" ~/ binary_digit.! ~/ (binary_digit | "_").rep.!)
    .map(SignedNumber.fromBinary)
  private val number = (decimal | hex | binary)

  val number16bit = number.flatMap(Number16.fromSignedNumber(_))
  val number8bit = number.flatMap(Number8.fromSignedNumber(_))
  val number4bit = number.flatMap(Number4.fromSignedNumber(_))
}
