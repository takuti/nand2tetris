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

  def commandType(): CommandType =
    command.charAt(0) match {
      case '@'  => A_COMMAND
      case '('  => L_COMMAND
      case _    => C_COMMAND
    }

  // A_COMMAND @Xxx and L_COMMAND (Xxx)
  def symbol(): String = take("[@(]([^)]+)\\)?+".r)

  // C_COMMAND dest=comp;jump
  def dest(): String = take("^([^=;]+)=.*".r)

  def comp(): String =
    if (command.indexOf('=') > 0) take(".+=([^;]+).*".r)
    else take("([^;]+).*".r)

  def jump(): String = take("[^;]+;?+(.*)".r)

  private def take(re: util.matching.Regex) =
    re findFirstMatchIn command match {
      case Some(m) => m.group(1)
      case None => ""
    }
}
