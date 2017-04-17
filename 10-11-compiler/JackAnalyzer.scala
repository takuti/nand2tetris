import java.io.{File,PrintWriter}

import jackanalizer._

object JackAnalyzer {
  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      System.err.println("You need to specify a .jack file or directory path.")
      System.exit(1)
    }

    val is_dir = !args(0).matches("(.*?)\\.jack")
    val in_filepaths = (
      if (is_dir) {
        val dirpath = (args{0} + "/").replaceAll("/+$", "/")
        val dir = new File(dirpath)
        val files_all = dir.listFiles
        files_all.collect {
          case f if ".*?\\.jack".r.findFirstIn(f.getName).isDefined => dirpath + f.getName
        }
      } else {
        Array(args(0))
      }
    )

    // Tokenizer: xxx.jack -> xxxT.xml
    for (in_filepath <- in_filepaths) {
      val writer = new PrintWriter(in_filepath.replaceAll("\\.jack", "T.xml"))
      val tokenizer = new JackTokenizer(in_filepath)

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

      writer.close
    }

    // CompilationEngine: xxx.jack -> xxx.xml
    for (in_filepath <- in_filepaths) {
      val out_filepath = in_filepath.replaceAll("\\.jack", ".xml")
      new CompilationEngineXML(in_filepath, out_filepath)
    }
  }
}
