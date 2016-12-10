package info.ditrapani.asm

object Main {
  type Bytes = Either[String, Seq[Byte]]

  def main(args: Array[String]): Unit = {
    process(args) match {
      case Help => printHelpText()
      case Error(msg) => printError(msg)
      case Good(bytes) => toStandardOut(bytes)
    }
  }

  def process(args: Array[String]): Result = {
    args.exists(arg => (arg == "--help" || arg == "-h")) match {
      case true => Help
      case false => processWithoutHelp(args) match {
        case Left(s) => Error(s)
        case Right(bytes) => Good(bytes)
      }
    }
  }

  private def processWithoutHelp(args: Array[String]): Bytes = {
    args.size match {
      case 0 => Left("Missing command line arguments; requires 1 or 2")
      case 1 => assemble(args(0))
      case 2 => textTiles2BinaryTiles(args)
      case _ => Left("Too many command line arguments; supply 1 or 2")
    }
  }


  private def printHelpText(): Unit = {
    val input_stream = getClass.getResourceAsStream("/help.md")
    val help_text = scala.io.Source.fromInputStream(input_stream).mkString
    println(help_text) // scalastyle:ignore regex
  }

  private def printError(msg: String): Unit = {
    println(msg) // scalastyle:ignore regex
    println("Run with --help or -h to see help text") // scalastyle:ignore regex
  }

  def assemble(file_name: String): Bytes = {
    parseFileArg(file_name, (content) => Assembler(content))
  }

  private def textTiles2BinaryTiles(args: Array[String]): Bytes = {
    args(0) match {
      case "-t" | "--tiles" =>
        parseFileArg(args(1), (content) => tiles.Tiles.parseStr(content))
      case _ =>
        Left(
          "The two argument form is to create binary tile sets, " +
          "the first argument must be -t or --tiles."
        )
    }
  }

  private def parseFileArg(file_name: String, call_back: String => Bytes): Bytes = {
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
