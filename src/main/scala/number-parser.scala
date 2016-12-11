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
  def fromHex(tuple: (String, String)): SignedNumber = {
    val (first_digit, digits) = tuple
    val clean = first_digit + digits.filter(_ != '_')
    SignedNumber(Plus, Integer.parseInt(clean, 16))
  }

  def fromDecimal(tuple: (Option[String], String, String)): SignedNumber = {
    val (sign_option, first_digit, digits) = tuple
    val clean = first_digit + digits.filter(_ != '_')
    SignedNumber(Sign.fromOption(sign_option), Integer.parseInt(clean))
  }

  def fromBinary(tuple: (String, String)): SignedNumber = {
    val (first_digit, digits) = tuple
    val clean = first_digit + digits.filter(_ != '_')
    SignedNumber(Plus, Integer.parseInt(clean, 2))
  }
}

sealed abstract class Number
final case class Number16(value: Int) extends Number
final case class Number8(value: Int) extends Number
final case class Number4(value: Int) extends Number

object Number16 {
  def fromSignedNumber(number: SignedNumber): Parser[Number16] =
    number.sign match {
      case Plus => (number.value > 0xFFFF) match {
        case false => Pass.map(_ => Number16(number.value))
        case true => Fail.opaque(
          s"Positive decimal number is too large; max is ${0xFFFF}"
        )
      }
      case Minus => (number.value > 32 * 1024) match {
        case false => Pass.map(_ => Number16((~number.value + 1) & 0xFFFF))
        case true => Fail.opaque(
          s"Negative number is too large; max is -${0x8000}"
        )
      }
    }
}

object Number8 {
  def fromSignedNumber(number: SignedNumber): Parser[Number8] =
    number.sign match {
      case Plus => (number.value > 0xFF) match {
        case false => Pass.map(_ => Number8(number.value))
        case true => Fail.opaque(
          s"Positive decimal number is too large; max is ${0xFF}"
        )
      }
      case Minus => (number.value > 0x80) match {
        case false => Pass.map(_ => Number8((~number.value + 1) & 0xFF))
        case true => Fail.opaque(
          s"Negative number is too large; max is -${0x80}"
        )
      }
    }
}

object Number4 {
  def fromSignedNumber(number: SignedNumber): Parser[Number4] =
    number.sign match {
      case Plus => (number.value > 0xF) match {
        case false => Pass.map(_ => Number4(number.value))
        case true => Fail.opaque(
          s"Positive decimal number is too large; max is ${0xF}"
        )
      }
      case Minus => (number.value > 0x8) match {
        case false => Pass.map(_ => Number4((~number.value + 1) & 0xF))
        case true => Fail.opaque(
          s"Negative number is too large; max is -${0x8}"
        )
      }
    }
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
