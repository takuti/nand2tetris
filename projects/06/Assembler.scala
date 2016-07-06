import java.io.PrintWriter

import assembler.{CommandType,A_COMMAND,C_COMMAND,L_COMMAND}
import assembler.{Parser,Code,SymbolTable}

object Assembler {
  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      System.err.println("You need to specify a .asm file.")
      System.exit(1)
    }

    // initialize symbol table with pre-defined symbols
    val st = new SymbolTable
    st.addEntry("SP", 0)
    st.addEntry("LCL", 1)
    st.addEntry("ARG", 2)
    st.addEntry("THIS", 3)
    st.addEntry("THAT", 4)
    for (n <- 0 to 15) st.addEntry("R" + n.toString, n)
    st.addEntry("SCREEN", 16384)
    st.addEntry("KBD", 24576)

    // first pass for symbol table construction
    var parser = new Parser(args{0})
    var address = 0

    while (parser.hasMoreCommands()) {
      parser.advance()

      if (!parser.command.isEmpty()) {
        val ctype: CommandType = parser.commandType()

        if (ctype == A_COMMAND || ctype == C_COMMAND) address += 1
        else st.addEntry(parser.symbol(), address) // L_COMMAND (Xxx)
      }
    }

    // open a writer for an output .hack file
    val pattern = "(.*?)\\.asm".r
    val output_filename = args{0} match {
      case pattern(name) => name + ".hack"
      case _             => System.exit(1)
    }
    val writer = new PrintWriter(output_filename.toString)

    // second pass for parsing with new parser
    parser = new Parser(args{0})
    val code = new Code
    var ram_address = 16

    while (parser.hasMoreCommands()) {
      parser.advance()

      if (!parser.command.isEmpty()) {
        val ctype: CommandType = parser.commandType()
        var line = ""

        if (ctype == A_COMMAND) {
          var symbol = parser.symbol()
          if (!symbol.matches("[0-9]+")) { // @Xxx (Xxx is not numeric)
            if (!st.contains(symbol)) {
              st.addEntry(symbol, ram_address)
              ram_address += 1
            }
            symbol = st.getAddress(symbol).toString
          }
          line = String.format("0%15s", Integer.toBinaryString(symbol.toInt)).replace(' ', '0')
        } else if (ctype == C_COMMAND) {
          line = "111" + code.comp(parser.comp()) + code.dest(parser.dest()) + code.jump(parser.jump())
        }

        if (ctype != L_COMMAND) writer.write(line + "\n")
      }
    }

    writer.close()
  }
}
