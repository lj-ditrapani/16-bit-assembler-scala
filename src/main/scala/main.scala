package info.ditrapani.asm

object Main {
  type Result = Either[String, Seq[Byte]]

  def main(args: Array[String]): Unit = {
    val result: Result = args.size match {
      case 0 => Left("Missing command line arguments; requires 1 or 2")
      case 1 => parseFileArgAndAssemble(args(0))
      case 2 => parseTilesArgs(args)
      case _ => Left("Too many command line arguments; supply 1 or 2")
    }
    result match {
      case Left(msg) => printHelpText(msg)
      case Right(binary) => toStandardOut(binary)
    }
  }

  def printHelpText(msg: String): Unit = {
    msg match {
      case "" => Unit
      case _ => println(msg + "\n") // scalastyle:ignore regex
    }
    val input_stream = getClass.getResourceAsStream("/help.md")
    val help_text = scala.io.Source.fromInputStream(input_stream).mkString
    println(help_text) // scalastyle:ignore regex
  }

  def parseFileArgAndAssemble(file_name: String): Result = {
    parseFileArg(file_name, (content) => Assembler(content))
  }

  def parseTilesArgs(args: Array[String]): Result = {
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

  def parseFileArg(file_name: String, call_back: String => Result): Result = {
    import scala.util.{Try, Success, Failure}

    Try(scala.io.Source.fromFile(file_name).mkString) match {
      case Failure(exception) => Left(exception.toString)
      case Success(str) => call_back(str)
    }
  }

  def toStandardOut(s: Seq[Byte]): Unit = {
    import java.io.BufferedOutputStream

    var out = new BufferedOutputStream(System.out)
    out.write(s.toArray)
    out.flush
  }
}
