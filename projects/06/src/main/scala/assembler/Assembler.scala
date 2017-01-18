import java.io.PrintWriter
import assembler._

object Assembler {
  def initializeSymbolTable(): SymbolTable = {
    val st = new SymbolTable

    // pre-defined symbols
    st.addEntry("SP", 0)
    st.addEntry("LCL", 1)
    st.addEntry("ARG", 2)
    st.addEntry("THIS", 3)
    st.addEntry("THAT", 4)
    for (n <- 0 to 15) st.addEntry("R" + n.toString, n)
    st.addEntry("SCREEN", 16384)
    st.addEntry("KBD", 24576)

    st
  }

  def firstPass(st: SymbolTable, filename: String) = {
    val parser = new Parser(filename)
    var address = 0

    // symbol table construction
    while (parser.hasMoreCommands()) {
      parser.advance()

      if (!parser.command.isEmpty()) {
        val ctype: CommandType = parser.commandType()

        if (ctype == A_COMMAND || ctype == C_COMMAND) address += 1
        else st.addEntry(parser.symbol(), address) // L_COMMAND (Xxx)
      }
    }
  }

  def secondPass(st: SymbolTable, in_filename: String, out_filename: String) = {
    val writer = new PrintWriter(out_filename)

    val parser = new Parser(in_filename)
    val code = new Code
    var ram_address = 16

    // parsing
    while (parser.hasMoreCommands()) {
      parser.advance()

      if (!parser.command.isEmpty()) {
        val ctype: CommandType = parser.commandType()

        if (ctype == A_COMMAND) {
          val symbol = parser.symbol()
          var addr = 0

          // @Xxx (Xxx is numeric, or not)
          if (symbol.matches("[0-9]+")) {
            addr = symbol.toInt
          } else {
            // add new label
            if (!st.contains(symbol)) {
              st.addEntry(symbol, ram_address)
              ram_address += 1
            }

            addr = st.getAddress(symbol)
          }

          writer.write(String.format("0%15s\n", Integer.toBinaryString(addr)).replace(' ', '0'))
        } else if (ctype == C_COMMAND) {
          writer.write("111" + code.comp(parser.comp()) + code.dest(parser.dest()) + code.jump(parser.jump()) + "\n")
        }
      }
    }

    writer.close()
  }

  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      System.err.println("You need to specify a .asm file.")
      System.exit(1)
    }

    val st: SymbolTable = initializeSymbolTable()

    firstPass(st, args{0})

    // get an output filename corresponding to the .hack files
    val pattern = "(.*?)\\.asm".r
    val out_filename = args{0} match {
      case pattern(name) => name + ".hack"
      case _             => System.exit(1)
    }

    secondPass(st, args{0}, out_filename.toString)
  }
}
