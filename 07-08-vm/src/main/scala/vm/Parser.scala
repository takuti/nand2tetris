package vmtranslator

import java.io.{BufferedReader,FileReader}

sealed abstract class CommandType
case object C_ARITHMETIC extends CommandType
case object C_PUSH extends CommandType
case object C_POP extends CommandType
case object C_LABEL extends CommandType
case object C_GOTO extends CommandType
case object C_IF extends CommandType
case object C_FUNCTION extends CommandType
case object C_RETURN extends CommandType
case object C_CALL extends CommandType

class Parser(_filename: String) {
  val reader = new BufferedReader(new FileReader(_filename))
  var command = ""

  def hasMoreCommands(): Boolean = reader.ready()

  // called when hasMoreCommands() returned true
  def advance() = {
    command = reader.readLine()

    // remove comment and head/tail whitespaces
    command = "\\/\\/.*".r.replaceAllIn(command, "").trim
  }

  def commandType(): CommandType = command.split("\\s+")(0) match {
    // get a `command` part from a full VM command
      case "push"     => C_PUSH
      case "pop"      => C_POP
      case "label"    => C_LABEL
      case "goto"     => C_GOTO
      case "if-goto"  => C_IF
      case "function" => C_FUNCTION
      case "call"     => C_CALL
      case "return"   => C_RETURN
      case _          => C_ARITHMETIC
    }

  // C_RETURN command cannot call this method
  def arg1(): String = {
    val cmd = command.split("\\s+")
    if (commandType() == C_ARITHMETIC) cmd(0)
    else command.split("\\s+")(1)
  }

  // Only C_PUSH, C_POP, C_FUNCTION and C_CALL can call this method
  def arg2(): Int = command.split("\\s+")(2).toInt
}
