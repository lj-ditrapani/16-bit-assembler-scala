package info.ditrapani.asm

object Main {
  def main(args: Array[String]): Unit = {
    args.size match {
      case 0 => printHelpText()
      case 1 => parseFileArgAndAssemble(args(0))
      case 2 => parseTilesArgs(args)
      case _ => printHelpText()
    }
  }

  def parseFileArgAndAssemble(file_name: String): Unit = {
    parseFileArg(file_name, (content) => Assembler(content))
  }

  def printHelpText(): Unit = {
    val input_stream = getClass.getResourceAsStream("/help.md")
    val help_text = scala.io.Source.fromInputStream(input_stream).mkString
    println(help_text)
  }

  def parseFileArg(file_name: String, call_back: String => Unit): Unit = {
    import scala.util.{Try, Success, Failure}

    Try(scala.io.Source.fromFile(file_name).mkString) match {
      case Failure(exception) => println(exception)
      case Success(str) => call_back(str)
    }
  }

  def parseTilesArgs(args: Array[String]): Unit = {
    args(0) match {
      case "-t" | "--tiles" => 
        parseFileArg(args(1), (content) => tiles.Tiles.parseStr(content))
      case _ =>
        printHelpText()
    }
  }
}
