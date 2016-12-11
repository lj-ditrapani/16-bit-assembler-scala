package info.ditrapani.asm.number

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

sealed abstract class Number
final case class Number16(value: Int) extends Number
final case class Number8(value: Int) extends Number
final case class Number4(value: Int) extends Number

class FromSignedNumber[T](max_positive: Int, max_negative: Int, contructor: Int => T) {
  def fromSignedNumber(number: SignedNumber): Parser[T] =
    number.sign match {
      case Plus => (number.value > max_positive) match {
        case false => Pass.map(_ => contructor(number.value))
        case true => Fail.opaque(
          s"Positive number is too large; max is ${max_positive}"
        )
      }
      case Minus => (number.value > max_negative) match {
        case false => Pass.map(_ => contructor((~number.value + 1) & max_positive))
        case true => Fail.opaque(
          s"Negative number is too large; max is -${max_negative}"
        )
      }
    }
}

sealed abstract class SignedNumberToNumber[T] {
  val converter: FromSignedNumber[T]

  def fromSignedNumber(number: SignedNumber): Parser[T] =
    converter.fromSignedNumber(number)
}

object Number16 extends SignedNumberToNumber[Number16] {
  val converter = new FromSignedNumber(0xFFFF, 0x8000, Number16.apply _)
}

object Number8 extends SignedNumberToNumber[Number8] {
  val converter = new FromSignedNumber(0xFF, 0x80, Number8.apply _)
}

object Number4 extends SignedNumberToNumber[Number4] {
  val converter = new FromSignedNumber(0xF, 0x8, Number4.apply _)
}

object NumberParser {
  val decimal_digit = P(CharIn('0' to '9'))
  val hex_letters = P(CharIn('a' to 'f') | CharIn('A' to 'F'))
  val hex_digit = P(decimal_digit | hex_letters)
  val binary_digit = P(CharIn("01"))
  val decimal = (("+" | "-").!.? ~ decimal_digit.! ~/ (decimal_digit | "_").rep.!)
    .map(SignedNumber.fromDecimal)
  val hex = P("$" ~/ hex_digit.! ~/ (hex_digit | "_").rep.!)
    .map(SignedNumber.fromHex)
  val binary = P("%" ~/ binary_digit.! ~/ (binary_digit | "_").rep.!)
    .map(SignedNumber.fromBinary)
  val number = (decimal | hex | binary)

  val number16bit = number.flatMap(Number16.fromSignedNumber(_))
  val number8bit = number.flatMap(Number8.fromSignedNumber(_))
  val number4bit = number.flatMap(Number4.fromSignedNumber(_))
}
