package jackanalizer

import java.io.PrintWriter

sealed abstract class Segment
case object CONST extends Segment { override def toString = "constant" }
case object ARG extends Segment { override def toString = "argument" }
case object LOCAL extends Segment { override def toString = "local" }
case object STATIC extends Segment { override def toString = "static" }
case object THIS extends Segment { override def toString = "this" }
case object THAT extends Segment { override def toString = "that" }
case object POINTER extends Segment { override def toString = "pointer" }
case object TEMP extends Segment { override def toString = "temp" }

sealed abstract class Command
case object ADD extends Command { override def toString = "add" }
case object SUB extends Command { override def toString = "sub" }
case object NEQ extends Command { override def toString = "neq" }
case object EQ extends Command { override def toString = "eq" }
case object GT extends Command { override def toString = "gt" }
case object LT extends Command { override def toString = "lt" }
case object AND extends Command { override def toString = "and" }
case object OR extends Command { override def toString = "or" }
case object NOT extends Command { override def toString = "not" }

class VMWriter(outFilePath: String) {
  val writer = new PrintWriter(outFilePath)

  def writePush(seg: Segment, idx: Int) = writer.write("push " + seg + " " + idx + "\n")

  def writePop(seg: Segment, idx: Int) = writer.write("pop " + seg + " " + idx + "\n")

  def writeArithmetic(cmd: Command) = writer.write(cmd + "\n")

  def writeLabel(label: String) = writer.write("label " + label + "\n")

  def writeGoto(label: String) = writer.write("goto " + label + "\n")

  def writeIf(label: String) = writer.write("if-goto " + label + "\n")

  def writeCall(name: String, nArgs: Int) = writer.write("call " + name + " " + nArgs + "\n")

  def writeFunction(name: String, nLocals: Int) = writer.write("function " + name + " " + nLocals + "\n")

  def writeReturn = writer.write("return" + "\n")

  def close = writer.close
}
