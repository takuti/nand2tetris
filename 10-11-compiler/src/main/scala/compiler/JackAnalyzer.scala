package compiler

import java.io.{File,PrintWriter}

object JackAnalyzer {
  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      System.err.println("You need to specify a .jack file or directory path.")
      System.exit(1)
    }

    val path = args(0)
    val isJack = path.matches("(.*?)\\.jack")
    val inFilepaths: Seq[String] =
      if (isJack) {
        Seq(path)
      } else {
        val file = new File(path)
        if (!file.isDirectory) throw new RuntimeException("Only .jack files or directories are acceptable")
        val allFiles = file.listFiles
        allFiles.collect {
          case f if ".*?\\.jack".r.findFirstIn(f.getName).isDefined => path + "/" + f.getName
        }
      }

    // Tokenizer: xxx.jack -> xxxT.xml
    for (inFilepath <- inFilepaths) {
      val writer = new PrintWriter(inFilepath.replaceAll("\\.jack", "T.xml"))
      val tokenizer = new JackTokenizer(inFilepath)

      writer.write("<tokens>\n")
      while (tokenizer.hasMoreTokens()) {
        tokenizer.advance()

        val tag = tokenizer.tokenType() match {
          case KEYWORD      => "keyword"
          case SYMBOL       => "symbol"
          case INT_CONST    => "integerConstant"
          case STRING_CONST => "stringConstant"
          case IDENTIFIER   => "identifier"
        }

        val value = tokenizer.tokenType() match {
          case KEYWORD      => tokenizer.token
          case SYMBOL       => tokenizer.token match {
                                 case "<"    => "&lt;"
                                 case ">"    => "&gt;"
                                 case "&"    => "&amp;"
                                 case _ @ t  => tokenizer.symbol()
                               }
          case INT_CONST    => tokenizer.intVal()
          case STRING_CONST => tokenizer.stringVal()
          case IDENTIFIER   => tokenizer.identifier()
        }

        writer.println(raw"""<${tag}> ${value} </${tag}>""")
      }
      writer.println("</tokens>")

      writer.close()
    }

    // CompilationEngine: xxx.jack -> xxx.xml
    for (inFilepath <- inFilepaths) {
      val outFilepath = inFilepath.replaceAll("\\.jack", ".xml")
      new CompilationEngineXML(inFilepath, outFilepath)
    }
  }
}
