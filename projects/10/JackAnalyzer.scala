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

    // xxx.jack -> xxxT.xml
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

        val token = tokenizer.token match {
          case "<"    => "&lt;"
          case ">"    => "&gt;"
          case "&"    => "&amp;"
          case _ @ t  => t
        }

        writer.write(raw"""<${tag}> ${token.replaceAll("\"", "")} </${tag}>""" + "\n")
      }
      writer.write("</tokens>\n")

      writer.close
    }

  }
}
