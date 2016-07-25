import java.io.File

import vmtranslator.{CommandType,C_ARITHMETIC,C_PUSH,C_POP,C_LABEL,C_GOTO,C_IF,C_FUNCTION,C_RETURN,C_CALL}
import vmtranslator.{Parser,CodeWriter}

object VMtranslator {
  def main(args: Array[String]): Unit = {

    if (args.size == 0) {
      System.err.println("You need to specify a .vm file or directory path.")
      System.exit(1)
    }

    val is_dir = !args{0}.matches("(.*?)\\.vm")
    var in_filenames = Array[String]()

    if (is_dir) {
      // need to translate all .vm files under a directory
      val dirname = args{0}.replaceAll("/+$", "/")
      val files_all = new File(dirname).listFiles
      val files_vm = files_all.filter(f => ".*?\\.vm".r.findFirstIn(f.getName).isDefined)
      for (f <- files_vm) in_filenames :+= dirname + f.getName
    } else {
      in_filenames :+= args{0}
    }

    val out_filename = if (is_dir) args{0}.replaceAll("/+$", "") + ".asm"
                       else args{0}.replaceAll("\\.vm", ".asm")
    val writer = new CodeWriter(out_filename)

    for (in_filename <- in_filenames) {
      val parser = new Parser(in_filename)
      writer.setFileName(in_filename)

      while (parser.hasMoreCommands()) {
        parser.advance()

        if (!parser.command.isEmpty()) {
          parser.commandType() match {
            case C_ARITHMETIC             => writer.writeArithmetic(parser.arg1())
            case ctype @ (C_PUSH | C_POP) => writer.writePushPop(ctype, parser.arg1(), parser.arg2())
            case C_LABEL                  => writer.writeLabel(parser.arg1())
            case C_GOTO                   => writer.writeGoto(parser.arg1())
            case C_IF                     => writer.writeIf(parser.arg1())
          }
        }
      }
    }

    writer.close()

  }
}
