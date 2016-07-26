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

  def writeLabel(label: String) = {
    writer.write(s"($label)\n")
  }

  def writeGoto(label: String) = {
    writer.write(s"@$label\n")
    writer.write("0;JMP\n")
  }

  def writeIf(label: String) = {
    // pop and keep it in D
    writePushPop(C_POP, "constant", 13)
    writer.write("@13\n")
    writer.write("D=M\n")

    // if popped value != 0 -> jump to the label
    writer.write(s"@$label\n")
    writer.write("D;JNE\n")
  }

  def writeCall(functionName: String, numArgs: Int) = {
  }

  def writeReturn() = {
    // M[13] = LCL(M[1])
    writer.write("@1\nD=M\n@13\nM=D\n")

    // write popped value to M[ ARG(M[2]) ]
    writePushPop(C_POP, "argument", 0)

    // SP(M[0]) = ARG(M[2]) + 1
    writer.write("@2\nD=M\n@0\nM=D+1\n")

    // THAT(M[4]) = M[ M[13] - 1 ]
    writer.write("@1\nD=A\n@13\nA=M-D\nD=M\n@4\nM=D\n")

    // THIS(M[3]) = M[ M[13] - 2 ]
    writer.write("@2\nD=A\n@13\nA=M-D\nD=M\n@3\nM=D\n")

    // ARG(M[2]) = M[ M[13] - 3 ]
    writer.write("@3\nD=A\n@13\nA=M-D\nD=M\n@2\nM=D\n")

    // LCL(M[1]) = M[ M[13] - 4 ]
    writer.write("@4\nD=A\n@13\nA=M-D\nD=M\n@1\nM=D\n")

    // goto the return address; save M[ M[13] - 5 ] to M[14]
    writer.write("@5\nD=A\n@13\nA=M-D\nD=M\n@14\nM=D\n")
  }

  def writeFunction(functionName: String, numLocals: Int) = {
    writeLabel(functionName)

    // initialize k local variables with 0
    for (k <- 0 until numLocals) writer.write(s"@$k\nD=A\n@1\nA=M+D\nM=0\n")
  }

  def close() = {
    writer.write("(END)\n")
    writer.write("@END\n")
    writer.write("0;JMP\n")
    writer.close()
  }
}
