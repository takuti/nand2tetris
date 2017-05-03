package vmtranslator

import java.io.File

object VMtranslator {
  def main(args: Array[String]): Unit = {

    if (args.size == 0) {
      System.err.println("You need to specify a .vm file or directory path.")
      System.exit(1)
    }

    val isVm = !args(0).matches("(.*?)\\.vm")
    var inFilenames = Seq[String]()

    val outFilename = if (isVm) {
        // need to translate all .vm files under a directory
        val dirpath = (args(0) + "/").replaceAll("/+$", "/")
        val dir = new File(dirpath)
        val allFiles = dir.listFiles
        val vmFiles = allFiles.filter(f => ".*?\\.vm".r.findFirstIn(f.getName).isDefined)
        vmFiles.foreach(f => inFilenames :+= dirpath + f.getName)

        dirpath + dir.getName + ".asm"
      } else {
        inFilenames :+= args(0)

        args(0).replaceAll("\\.vm", ".asm")
      }

    val writer = new CodeWriter(outFilename)

    // bootstrap code if Sys.vm exists
    if (inFilenames.filter(name => ".*?\\Sys.vm".r.findFirstIn(name).nonEmpty).nonEmpty) {
      writer.writer.write("@256\nD=A\n@0\nM=D\n") // SP = 256
      writer.writeCall("Sys.init", 0) // call Sys.init
    }

    for (inFilename <- inFilenames) {
      val parser = new Parser(inFilename)
      writer.setFileName(inFilename)

      while (parser.hasMoreCommands()) {
        parser.advance()

        if (!parser.command.isEmpty) {
          parser.commandType() match {
            case C_ARITHMETIC             => writer.writeArithmetic(parser.arg1())
            case ctype @ (C_PUSH | C_POP) => writer.writePushPop(ctype, parser.arg1(), parser.arg2())
            case C_LABEL                  => writer.writeLabel(parser.arg1())
            case C_GOTO                   => writer.writeGoto(parser.arg1())
            case C_IF                     => writer.writeIf(parser.arg1())
            case C_FUNCTION               => writer.writeFunction(parser.arg1(), parser.arg2())
            case C_CALL                   => writer.writeCall(parser.arg1(), parser.arg2())
            case C_RETURN                 => writer.writeReturn()
          }
        }
      }
    }

    writer.close()
  }
}
