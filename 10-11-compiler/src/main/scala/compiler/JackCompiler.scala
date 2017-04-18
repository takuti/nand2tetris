package compiler

import java.io.{File,PrintWriter}

object JackCompiler {
  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      System.err.println("You need to specify a .jack file or directory path.")
      System.exit(1)
    }

    val isDir = !args(0).matches("(.*?)\\.jack")
    val inFilepaths: Seq[String] = if (isDir) {
        val dirpath = (args(0) + "/").replaceAll("/+$", "/")
        val dir = new File(dirpath)
        val allFiles = dir.listFiles
        allFiles.collect {
          case f if ".*?\\.jack".r.findFirstIn(f.getName).isDefined => dirpath + f.getName
        }
      } else {
        Seq(args(0))
      }

    // CompilationEngine: xxx.jack -> xxx.xml
    for (inFilepath <- inFilepaths) {
      val outFilepath = inFilepath.replaceAll("\\.jack", ".vm")
      new CompilationEngine(inFilepath, outFilepath)
    }
  }
}
