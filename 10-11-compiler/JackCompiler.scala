import java.io.{File,PrintWriter}

import jackanalizer._

object JackCompiler {
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

    // CompilationEngine: xxx.jack -> xxx.xml
    for (in_filepath <- in_filepaths) {
      val out_filepath = in_filepath.replaceAll("\\.jack", ".vm")
      new CompilationEngine(in_filepath, out_filepath)
    }
  }
}
