package assembler

import java.io.{BufferedReader,FileReader}

sealed abstract class CommandType
case object A_COMMAND extends CommandType
case object C_COMMAND extends CommandType
case object L_COMMAND extends CommandType

class Parser(_filename: String) {
  val reader = new BufferedReader(new FileReader(_filename))
  var command = ""

  def hasMoreCommands(): Boolean = reader.ready()

  // called when hasMoreCommands() returned true
  def advance() = {
    command = reader.readLine()

    // remove comment
    command = "\\/\\/.*".r.replaceAllIn(command, "")

    // remove whitespaces
    command = "\\s".r.replaceAllIn(command, "")
  }

  def commandType(): CommandType = {
    command.charAt(0) match {
      case '@'  => A_COMMAND
      case '('  => L_COMMAND
      case _    => C_COMMAND
    }
  }

  // A_COMMAND @Xxx and L_COMMAND (Xxx)
  def symbol(): String = {
    val pattern = "[@(]([^)]+)\\)?+".r
    command match { case pattern(xxx) => xxx }
  }

  // C_COMMAND dest=comp;jump
  def dest(): String = {
    var dst = ""
    if (command.indexOf('=') > 0) {
      val pattern = "([^=;]+).*".r
      dst = command match { case pattern(dst) => dst }
    }
    dst
  }

  def comp(): String = {
    val pattern = if (command.indexOf('=') > 0) ".+=([^;]+).*".r else "([^;]+).*".r
    command match { case pattern(cmp) => cmp }
  }

  def jump(): String = {
    val pattern = "[^;]+;?+(.*)".r
    command match { case pattern(jmp) => jmp }
  }
}
