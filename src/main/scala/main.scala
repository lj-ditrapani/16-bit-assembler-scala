package info.ditrapani.asm

object Main {
  type EResult = Either[String, Seq[Byte]]

  def main(args: Array[String]): Unit = {
    process(args) match {
      case Help => printHelpText("")
      case Error(msg) => printHelpText(msg)
      case Good(binary) => toStandardOut(binary)
    }
  }

  def process(args: Array[String]): Result = {
    args.size match {
      case 0 => Error("Missing command line arguments; requires 1 or 2")
      case 1 => assemble(args(0))
      case 2 => textTiles2BinaryTiles(args)
      case _ => Error("Too many command line arguments; supply 1 or 2")
    }
  }

  private def printHelpText(msg: String): Unit = {
    msg match {
      case "" => Unit
      case _ => println(msg + "\n") // scalastyle:ignore regex
    }
    val input_stream = getClass.getResourceAsStream("/help.md")
    val help_text = scala.io.Source.fromInputStream(input_stream).mkString
    println(help_text) // scalastyle:ignore regex
  }

  def assemble(file_name: String): Result = {
    parseFileArg(file_name, (content) => Assembler(content)) match {
      case Left(s) => Error(s)
      case Right(s) => Good(s)
    }
  }

  private def textTiles2BinaryTiles(args: Array[String]): Result = {
    args(0) match {
      case "-t" | "--tiles" =>
        parseFileArg(args(1), (content) => tiles.Tiles.parseStr(content)) match {
          case Left(s) => Error(s)
          case Right(s) => Good(s)
        }
      case _ =>
        Error(
          "The two argument form is to create binary tile sets, " +
          "the first argument must be -t or --tiles."
        )
    }
  }

  private def parseFileArg(file_name: String, call_back: String => EResult): EResult = {
    import scala.util.{Try, Success, Failure}

    Try(scala.io.Source.fromFile(file_name).mkString) match {
      case Failure(exception) => Left(exception.toString)
      case Success(str) => call_back(str)
    }
  }

  private def toStandardOut(s: Seq[Byte]): Unit = {
    import java.io.BufferedOutputStream

    val out = new BufferedOutputStream(System.out)
    out.write(s.toArray)
    out.flush
  }
}
