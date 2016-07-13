package vmtranslator

import java.io.PrintWriter
import java.io.File

class CodeWriter(_filename: String) {
  val writer = new PrintWriter(_filename)

  var current_vmname = "" // for static variables
  var cmp_cnt = 0 // for arithmetic commands with comparison (i.e. jumping w/ label)

  def setFileName(filename: String) = {
    val file = new File(filename)
    current_vmname = file.getName.replaceAll("\\.vm", "")
    println(current_vmname)
  }

  def writeArithmetic(command: String) = {
    val unary_functions = Array("neg", "not")
    val bool_functions = Array("eq", "gt", "lt")

    writePushPop(C_POP, "constant", 13) // y

    if (unary_functions contains command) {
      writer.write("@13\n")

      command match {
        case "neg" => writer.write("D=-M\n")
        case "not" => writer.write("D=!M\n")
      }
    } else { // binary functions
      writePushPop(C_POP, "constant", 14) // x
      writer.write("@14\n")
      writer.write("D=M\n")
      writer.write("@13\n")

      if (bool_functions contains command) {
        val cmp = command match {
          case "eq"  => "JEQ"
          case "gt"  => "JGT"
          case "lt"  => "JLT"
        }
        writer.write("D=D-M\n")
        writer.write(s"@T$cmp_cnt\n")
        writer.write(s"D;$cmp\n")
        writer.write("D=0\n")
        writer.write(s"@PUSH$cmp_cnt\n")
        writer.write("0;JMP\n")
        writer.write(s"(T$cmp_cnt)\n")
        writer.write("D=-1\n")
        writer.write(s"(PUSH$cmp_cnt)\n")
        cmp_cnt += 1
      } else {
        command match {
          case "add" => writer.write("D=D+M\n")
          case "sub" => writer.write("D=D-M\n")
          case "and" => writer.write("D=D&M\n")
          case "or"  => writer.write("D=D|M\n")
        }
      }
    }

    writer.write("@0\nM=M+1\nA=M-1\nM=D\n")
  }

  def writePushPop(command: CommandType, segment: String, index: Int) = {

    val access_seg_i = segment match {
      case "constant" => s"@$index\n"
      case "local"    => s"@$index\nD=A\n@1\nA=M+D\n"
      case "argument" => s"@$index\nD=A\n@2\nA=M+D\n"
      case "this"     => s"@$index\nD=A\n@3\nA=M+D\n"
      case "that"     => s"@$index\nD=A\n@4\nA=M+D\n"
      case "pointer"  => s"@$index\nD=A\n@3\nA=A+D\n"
      case "temp"     => s"@$index\nD=A\n@5\nA=A+D\n"
      case "static"   => s"@$current_vmname.$index\n"
    }

    // only support `constant` segment
    if (command == C_PUSH) {
      // write to SP and SP++
      writer.write(access_seg_i)

      segment match {
        case "constant" => writer.write("D=A\n")
        case _          => writer.write("D=M\n")
      }

      writer.write("@0\n")
      writer.write("M=M+1\n")
      writer.write("A=M-1\n")
      writer.write("M=D\n")
    } else if (command == C_POP) {
      // SP-- and read value
      writer.write(access_seg_i)
      writer.write("D=A\n")
      writer.write("@15\n") // keep dist address
      writer.write("M=D\n")
      writer.write("@0\n") // SP-- and get popped value
      writer.write("M=M-1\n")
      writer.write("A=M\n")
      writer.write("D=M\n")
      writer.write("@15\n")
      writer.write("A=M\n") // move to the dist address
      writer.write("M=D\n")
    }
  }

  def close() = {
    writer.write("(END)\n")
    writer.write("@END\n")
    writer.write("0;JMP\n")
    writer.close()
  }
}
