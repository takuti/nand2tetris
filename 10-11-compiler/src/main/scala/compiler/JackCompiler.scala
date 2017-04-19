package compiler

import java.io.{File,PrintWriter}

object JackCompiler {
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

    // CompilationEngine: xxx.jack -> xxx.xml
    for (inFilepath <- inFilepaths) {
      val outFilepath = inFilepath.replaceAll("\\.jack", ".vm")
      new CompilationEngine(inFilepath, outFilepath)
    }
  }
}
