package assembler

import java.io.PrintWriter

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

      if (!parser.command.isEmpty) {
        parser.commandType() match {
          case A_COMMAND | C_COMMAND => address += 1
          case _ => st.addEntry(parser.symbol(), address) // L_COMMAND (Xxx)
        }
      }
    }
  }

  def secondPass(st: SymbolTable, inFilename: String, outFilename: String) = {
    val writer = new PrintWriter(outFilename)

    val parser = new Parser(inFilename)
    val code = new Code
    var ramAddress = 16

    // parsing
    while (parser.hasMoreCommands()) {
      parser.advance()

      if (!parser.command.isEmpty) {
        parser.commandType() match {
          case A_COMMAND => {
            val numPattern = "([0-9]+)".r
            val addr = parser.symbol() match {
              case numPattern(symbol) =>
                symbol.toInt
              case _ @ symbol => {
                // add new label
                if (!st.contains(symbol)) {
                  st.addEntry(symbol, ramAddress)
                  ramAddress += 1
                }
                st.getAddress(symbol)
              }
            }
            writer.write(String.format("0%15s\n", Integer.toBinaryString(addr))
              .replace(' ', '0'))
          }
          case C_COMMAND =>
            writer.write("111"
              + code.comp(parser.comp())
              + code.dest(parser.dest())
              + code.jump(parser.jump()) + "\n")
          case _ =>
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

    firstPass(st, args(0))

    // get an output filename corresponding to the .hack files
    val pattern = "(.*?)\\.asm".r
    val outFilename: String = args(0) match {
      case pattern(name) => name + ".hack"
      case _             => throw new RuntimeException
    }

    secondPass(st, args(0), outFilename)
  }
}
