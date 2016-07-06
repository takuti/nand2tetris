import java.io.PrintWriter

import assembler.{CommandType,A_COMMAND,C_COMMAND,L_COMMAND}
import assembler.{Parser,Code}

object Assembler {
  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      System.err.println("You need to specify a .asm file.")
      System.exit(1)
    }

    // open a writer for an output .hack file
    val pattern = "(.*?)\\.asm".r
    val output_filename = args{0} match {
      case pattern(name) => name + ".hack"
      case _             => System.exit(1)
    }
    val writer = new PrintWriter(output_filename.toString())

    val parser = new Parser(args{0})
    val code = new Code

    while (parser.hasMoreCommands()) {
      parser.advance()

      if (!parser.command.isEmpty()) {
        val ctype: CommandType = parser.commandType()
        var line = ""

        // assume no L_COMMAND; symbols indicate decimal values
        if (ctype == A_COMMAND) {
          line = String.format("0%15s", Integer.toBinaryString(parser.symbol().toInt)).replace(' ', '0')
        } else if (ctype == C_COMMAND) {
          line = "111" + code.comp(parser.comp()) + code.dest(parser.dest()) + code.jump(parser.jump())
        }

        writer.write(line + "\n")
      }
    }

    writer.close()
  }
}
