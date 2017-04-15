package jackanalizer

import java.io.{BufferedReader,FileReader,PrintWriter}
import jackanalizer._

class CompilationEngine(_in_filepath: String, _out_filepath: String) {
  val tokenizer = new JackTokenizer(_in_filepath)
  val writer = new VMWriter(_out_filepath)
  val symbolTable = new SymbolTable()

  var className = ""

  // for nested labels
  var nIf = 0
  var nLoop = 0

  // for each `compileXX` methods...
  // Rule #1: "advance" for the 1st grammer element is done by caller
  // Rule #2: each grammer finally does "advance" for the next unknown token
  if (tokenizer.hasMoreTokens()) {
    tokenizer.advance() // class
    if (tokenizer.tokenType() == KEYWORD &&
        tokenizer.keyWord() == CLASS) compileClass()
  }

  writer.close()

  def getToken(): String = {
    tokenizer.tokenType() match {
      case KEYWORD      => tokenizer.token
      case SYMBOL       => tokenizer.symbol().toString
      case INT_CONST    => tokenizer.intVal().toString
      case STRING_CONST => tokenizer.stringVal()
      case IDENTIFIER   => tokenizer.identifier()
    }
  }

  def compileClass() {
    // class

    tokenizer.advance() // className
    className = getToken()

    tokenizer.advance() // {

    tokenizer.advance()

    while (tokenizer.tokenType() == KEYWORD &&
           Seq(STATIC, FIELD).contains(tokenizer.keyWord())) {
      compileClassVarDec()
    }

    while (tokenizer.tokenType() == KEYWORD &&
           Seq(CONSTRUCTOR, FUNCTION, METHOD).contains(tokenizer.keyWord())) {
      compileSubroutine()
    }

    // }
  }

  def compileClassVarDec() {
    val kind = getToken() match { // 'static' | 'field'
      case "static" => StaticKind
      case "field" => FieldKind
    }

    tokenizer.advance() // type
    val stype = getToken()

    tokenizer.advance() // varName
    symbolTable.define(getToken(), stype, kind)

    tokenizer.advance()
    while (tokenizer.tokenType() == SYMBOL &&
           tokenizer.symbol() == ',') {
      // ,

      tokenizer.advance() // varName having same kind & type
      symbolTable.define(getToken(), stype, kind)

      tokenizer.advance()
    }

    // ;

    tokenizer.advance()
  }

  def compileSubroutine() {
    symbolTable.startSubroutine()

    val funcType = getToken() // 'constructor' | 'function' | 'method'

    tokenizer.advance() // 'void' | type
    val returnType = getToken()

    tokenizer.advance() // subroutineName
    val subroutineName = getToken()

    tokenizer.advance() // (

    tokenizer.advance()

    if (funcType == "method") {
      // arg[0] is set to "this", so thre are k+1 arguments in total
      symbolTable.define("this", className, ArgumentKind)
    }

    compileParameterList()

    val nLocals = symbolTable.varCount(VarKind)
    writer.writeFunction(className + "." + subroutineName, nLocals)

    // "this" points to the current object
    if (funcType == "method") {
      writer.writePush(ArgSegment, 0)
      writer.writePop(PointerSegment, 0)
    } else if (funcType == "constructor") {
      writer.writePush(ThisSegment, 0)
      writer.writePop(PointerSegment, 0)
    }

    // )

    tokenizer.advance() // {

    tokenizer.advance()
    while (tokenizer.tokenType() == KEYWORD &&
           tokenizer.keyWord() == VAR) compileVarDec()

    compileStatements()

    // }

    tokenizer.advance()
  }

  def compileParameterList() {
    val type_keywords = Seq(INT, CHAR, BOOLEAN)
    var stype = ""
    var name = ""
    if (tokenizer.tokenType() == IDENTIFIER ||
        (tokenizer.tokenType() == KEYWORD &&
         type_keywords.contains(tokenizer.keyWord()))) {
      stype = getToken() // type

      tokenizer.advance() // varName
      name = getToken()

      symbolTable.define(name, stype, ArgumentKind)

      tokenizer.advance()
      while (tokenizer.tokenType() == SYMBOL &&
             tokenizer.symbol() == ',') {
        // ,

        tokenizer.advance() // type
        stype = getToken()

        tokenizer.advance() // varName
        name = getToken()

        symbolTable.define(name, stype, ArgumentKind)

        tokenizer.advance()
      }
    }
  }

  def compileVarDec() {
    // 'var'

    tokenizer.advance() // type
    val stype = getToken()

    tokenizer.advance() // varName

    symbolTable.define(getToken(), stype, VarKind)

    tokenizer.advance()
    while (tokenizer.tokenType() == SYMBOL &&
           tokenizer.symbol() == ',') {
      // ,

      tokenizer.advance() // varName

      symbolTable.define(getToken(), stype, VarKind)

      tokenizer.advance()
    }

    // ;

    tokenizer.advance()
  }

  def compileStatements() {
    while (tokenizer.tokenType() == KEYWORD &&
           Seq(LET, IF, WHILE, DO, RETURN).contains(tokenizer.keyWord())) {
      tokenizer.keyWord() match {
        case LET    => compileLet()
        case IF     => compileIf()
        case WHILE  => compileWhile()
        case DO     => compileDo()
        case RETURN => compileReturn()
        case _      =>
      }
    }
  }

  def compileDo() {
    // 'do'

    tokenizer.advance()
    compileSubroutineCall()

    writer.writePop(TempSegment, 0)

    // ;

    tokenizer.advance()
  }

  def compileSubroutineCall() {
    // subroutineName | className | varName
    var subroutineName = getToken()

    tokenizer.advance()
    if (tokenizer.symbol() == '.') {
      subroutineName += "." // .

      tokenizer.advance()
      subroutineName += getToken() // subroutineName

      tokenizer.advance()
    }

    // (

    tokenizer.advance()
    val nArgs = compileExpressionList()

    writer.writeCall(subroutineName, nArgs)

    // )

    tokenizer.advance()
  }

  def compileLet() {
    // 'let'

    tokenizer.advance()
    val varName = getToken() // varName

    tokenizer.advance()
    val seg = if (tokenizer.tokenType() == SYMBOL && tokenizer.symbol() == '[') {
      // array access

      // '['

      tokenizer.advance()
      compileExpression()

      // ']'

      tokenizer.advance()

      writer.writePop(PointerSegment, 1)

      ThatSegment
    } else {
      symbolTable.kindOf(varName) match {
        case StaticKind => StaticSegment
        case FieldKind => ThisSegment
        case ArgumentKind => ArgSegment
        case VarKind => LocalSegment
        case NoneKind => TempSegment
      }
    }

    // '='

    tokenizer.advance()
    compileExpression()

    // ';'

    if (seg == ThatSegment) writer.writePop(ThatSegment, 0)
    else writer.writePop(seg, symbolTable.indexOf(varName))

    tokenizer.advance()
  }

  def compileWhile() {
    // 'while'
    nLoop += 1

    writer.writeLabel("LOOP_START_" + nLoop)

    tokenizer.advance()
    // '('

    tokenizer.advance()
    compileExpression()

    // ')'

    writer.writeIf("LOOP_INSIDE_" + nLoop)
    writer.writeGoto("LOOP_END_" + nLoop)
    writer.writeLabel("LOOP_INSIDE_" + nLoop)

    tokenizer.advance()
    // '{'

    tokenizer.advance()
    compileStatements()

    // '}'

    writer.writeLabel("LOOP_END_" + nLoop)

    nLoop -= 1

    tokenizer.advance()
  }

  def compileReturn() {
    // 'return'

    tokenizer.advance()
    if (tokenizer.tokenType() != SYMBOL ||
        (tokenizer.tokenType() == SYMBOL &&
         tokenizer.symbol() != ';')) compileExpression()
    else writer.writePush(ConstSegment, 0)

    writer.writeReturn()

    tokenizer.advance()
  }

  def compileIf() {
    // 'if'
    nIf += 1

    tokenizer.advance()
    // '('

    tokenizer.advance()
    compileExpression()

    // ')'

    writer.writeIf("IF_TRUE_" + nIf)
    writer.writeGoto("IF_FALSE_" + nIf)
    writer.writeLabel("IF_TRUE_" + nIf)

    tokenizer.advance()
    // '{'

    tokenizer.advance()
    compileStatements()

    // '}'

    writer.writeLabel("IF_FALSE_" + nIf)

    nIf -= 1

    tokenizer.advance()
  }

  def compileExpression() {
    compileTerm()

    val ops = Seq('+', '-', '*', '/', '&', '|', '<', '>', '=')
    while (tokenizer.tokenType() == SYMBOL &&
           ops.contains(getToken().charAt(0))) {

      val op = getToken().charAt(0)

      tokenizer.advance()
      compileTerm()

      // op
      if (op == '+') writer.writeArithmetic(ADD)
      else if (op == '-') writer.writeArithmetic(SUB)
      else if (op == '*') writer.writeCall("Math.multiply", 2)
      else if (op == '/') writer.writeCall("Math.divide", 2)
      else if (op == '&') writer.writeArithmetic(AND)
      else if (op == '|') writer.writeArithmetic(OR)
      else if (op == '<') writer.writeArithmetic(LT)
      else if (op == '>') writer.writeArithmetic(GT)
      else if (op == '=') writer.writeArithmetic(EQ)
    }
  }

  def compileTerm() {
    tokenizer.tokenType() match {
      case SYMBOL => {
        tokenizer.symbol() match {
          case '('       => // '('
                            tokenizer.advance()
                            compileExpression()
                            // ')'
                            tokenizer.advance()
          case '-'       => // '-'
                            tokenizer.advance()
                            compileTerm()
                            writer.writeArithmetic(NEG)
          case '~' => // '~'
                            tokenizer.advance()
                            compileTerm()
                            writer.writeArithmetic(NOT)
        }
      }
      case KEYWORD => {
        val kw = tokenizer.keyWord()
        if (kw == TRUE) {
          writer.writePush(ConstSegment, 1)
          writer.writeArithmetic(NEG)
          tokenizer.advance()
        } else if (kw == FALSE || kw == NULL) {
          writer.writePush(ConstSegment, 0)
          tokenizer.advance()
        } else if (kw == THIS) {
          writer.writePush(ArgSegment, 0)
          tokenizer.advance()
        }
      }
      case STRING_CONST => {
        val str = getToken()
        writer.writePush(ConstSegment, str.length)
        writer.writeCall("String.new", 1)
        str.foreach { c =>
            writer.writePush(ConstSegment, c.toInt)
            writer.writeCall("String.appendChar", 1)
        }
        tokenizer.advance()
      }
      case INT_CONST => {
        writer.writePush(ConstSegment, getToken().toInt)
        tokenizer.advance()
      }
      case IDENTIFIER => {
        // varName | subroutineName
        var name = getToken()
        tokenizer.advance()

        if (tokenizer.tokenType() == SYMBOL) {
          if (Seq('.', '(').contains(tokenizer.symbol())) {
            // subroutineCall

            if (tokenizer.symbol() == '.') {
              // .

              tokenizer.advance()
              name = name + "." + getToken() // subroutineName

              tokenizer.advance()
            }

            // (

            tokenizer.advance()
            val nExpr = compileExpressionList()
            writer.writeCall(name, nExpr)

            // )
            tokenizer.advance()
          } else if (tokenizer.symbol() == '[') {
            // varName '[' expression ']' (array access)

            // '['

            tokenizer.advance()
            compileExpression()

            // ']'

            writer.writePop(PointerSegment, 1)
            writer.writePush(ThatSegment, 0)

            tokenizer.advance()
          }
        }
      }
    }
  }

  def compileExpressionList(): Int = {
    var nExpr = 0

    // non-empty expressionList
    if (tokenizer.tokenType() != SYMBOL ||
        (tokenizer.tokenType() == SYMBOL &&
         tokenizer.symbol() != ')')) {

      compileExpression()
      nExpr += 1

      while (tokenizer.tokenType() == SYMBOL &&
             tokenizer.symbol() == ',') {
        // ','

        tokenizer.advance()
        compileExpression()
        nExpr += 1
      }
    }

    nExpr
  }
}
