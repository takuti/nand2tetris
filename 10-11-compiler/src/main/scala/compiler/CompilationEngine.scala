package compiler

import java.io.{BufferedReader,FileReader,PrintWriter}

class CompilationEngine(_inFilepath: String, _outFilepath: String) {
  val tokenizer = new JackTokenizer(_inFilepath)
  val writer = new VMWriter(_outFilepath)
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

  def getToken(): String = tokenizer.tokenType() match {
      case KEYWORD      => tokenizer.token
      case SYMBOL       => tokenizer.symbol().toString
      case INT_CONST    => tokenizer.intVal().toString
      case STRING_CONST => tokenizer.stringVal()
      case IDENTIFIER   => tokenizer.identifier()
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
    nIf = 0
    nLoop = 0

    symbolTable.startSubroutine()

    val funcType = getToken() // 'constructor' | 'function' | 'method'

    if (funcType == "method") {
      // arg[0] is set to "this", so thre are k+1 arguments in total
      symbolTable.define("this", className, ArgumentKind)
    }

    tokenizer.advance() // 'void' | type
    val returnType = getToken()

    tokenizer.advance() // subroutineName
    val subroutineName = getToken()

    tokenizer.advance() // (

    tokenizer.advance()

    compileParameterList()

    // )

    tokenizer.advance() // {

    tokenizer.advance()
    while (tokenizer.tokenType() == KEYWORD &&
           tokenizer.keyWord() == VAR) compileVarDec()

    val nLocals = symbolTable.varCount(VarKind)
    writer.writeFunction(className + "." + subroutineName, nLocals)

    // "this" points to the current object
    if (funcType == "method") {
      writer.writePush(ArgSegment, 0)
      writer.writePop(PointerSegment, 0)
    } else if (funcType == "constructor") {
      writer.writePush(ConstSegment, symbolTable.varCount(FieldKind))
      writer.writeCall("Memory.alloc", 1)

      writer.writePop(PointerSegment, 0)
    }

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
    var isVar = false
    var nArgs = 0

    tokenizer.advance()
    if (tokenizer.symbol() == '.') { // className | varName
      if (symbolTable.kindOf(subroutineName) != NoneKind) { // varName
        isVar = true
        val seg = symbolTable.kindOf(subroutineName) match {
          case StaticKind => StaticSegment
          case FieldKind => ThisSegment
          case ArgumentKind => ArgSegment
          case VarKind => LocalSegment
          case _ => throw new RuntimeException
        }
        writer.writePush(seg, symbolTable.indexOf(subroutineName))
        subroutineName = symbolTable.typeOf(subroutineName) // replace with its class name
      }

      subroutineName += "." // .

      tokenizer.advance()
      subroutineName += getToken() // subroutineName

      tokenizer.advance()
    } else { // push `this` for internal subroutine
      writer.writePush(PointerSegment, 0)
      subroutineName = className + "." + subroutineName
      nArgs += 1
    }

    // (

    tokenizer.advance()
    nArgs += compileExpressionList()

    if (isVar) {
      writer.writeCall(subroutineName, nArgs + 1)
    } else {
      writer.writeCall(subroutineName, nArgs)
    }

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
      //
      val arraySeg = symbolTable.kindOf(varName) match {
        case StaticKind => StaticSegment
        case FieldKind => ThisSegment
        case ArgumentKind => ArgSegment
        case VarKind => LocalSegment
        case NoneKind => TempSegment
      }

      // '['

      tokenizer.advance()
      compileExpression()

      // ']'

      tokenizer.advance()

      writer.writePush(arraySeg, symbolTable.indexOf(varName))
      writer.writeArithmetic(ADD)

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

    if (seg == ThatSegment) {
      writer.writePop(TempSegment, 0)
      writer.writePop(PointerSegment, 1)
      writer.writePush(TempSegment, 0)
      writer.writePop(ThatSegment, 0)
    } else {
      writer.writePop(seg, symbolTable.indexOf(varName))
    }

    tokenizer.advance()
  }

  def compileWhile() {
    // 'while'
    val nLoopLocal = nLoop
    nLoop += 1

    writer.writeLabel("WHILE_EXP" + nLoopLocal)

    tokenizer.advance()
    // '('

    tokenizer.advance()
    compileExpression()

    // ')'

    writer.writeArithmetic(NOT)

    writer.writeIf("WHILE_END" + nLoopLocal)

    tokenizer.advance()
    // '{'

    tokenizer.advance()
    compileStatements()

    // '}'

    writer.writeGoto("WHILE_EXP" + nLoopLocal)

    writer.writeLabel("WHILE_END" + nLoopLocal)

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
    val nIfLocal = nIf
    nIf += 1

    tokenizer.advance()
    // '('

    tokenizer.advance()
    compileExpression()

    // ')'

    writer.writeIf("IF_TRUE" + nIfLocal)
    writer.writeGoto("IF_FALSE" + nIfLocal)
    writer.writeLabel("IF_TRUE" + nIfLocal)

    tokenizer.advance()
    // '{'

    tokenizer.advance()
    compileStatements()

    // '}'

    tokenizer.advance()
    if (tokenizer.tokenType() == KEYWORD && tokenizer.keyWord() == ELSE) {
      // w/ `else` statement
      writer.writeGoto("IF_END" + nIfLocal)
      writer.writeLabel("IF_FALSE" + nIfLocal)

      tokenizer.advance()
      // '{'

      tokenizer.advance()
      compileStatements()

      // '}'

      tokenizer.advance()

      writer.writeLabel("IF_END" + nIfLocal)
    } else {
      // w/o `else` statement
      writer.writeLabel("IF_FALSE" + nIfLocal)
    }
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
          writer.writePush(ConstSegment, 0)
          writer.writeArithmetic(NOT)
          tokenizer.advance()
        } else if (kw == FALSE || kw == NULL) {
          writer.writePush(ConstSegment, 0)
          tokenizer.advance()
        } else if (kw == THIS) {
          writer.writePush(PointerSegment, 0)
          tokenizer.advance()
        }
      }
      case STRING_CONST => {
        val str = getToken()
        writer.writePush(ConstSegment, str.length)
        writer.writeCall("String.new", 1)
        str.foreach { c =>
            writer.writePush(ConstSegment, c.toInt)
            writer.writeCall("String.appendChar", 2)
        }
        tokenizer.advance()
      }
      case INT_CONST => {
        writer.writePush(ConstSegment, getToken().toInt)
        tokenizer.advance()
      }
      case IDENTIFIER => {
        // varName | subroutineName
        var isVarCall = false
        var name = getToken()
        tokenizer.advance()

        if (tokenizer.tokenType() == SYMBOL) {
          if (Seq('.', '(').contains(tokenizer.symbol())) {
            // subroutineCall

            if (tokenizer.symbol() == '.') {
              // .
              if (symbolTable.kindOf(name) != NoneKind) { // varName
                isVarCall = true
                val seg = symbolTable.kindOf(name) match {
                  case StaticKind => StaticSegment
                  case FieldKind => ThisSegment
                  case ArgumentKind => ArgSegment
                  case VarKind => LocalSegment
                  case _ => throw new RuntimeException
                }
                writer.writePush(seg, symbolTable.indexOf(name))
                name = symbolTable.typeOf(name) // replace with its class name
              }

              tokenizer.advance()
              name = name + "." + getToken() // subroutineName

              tokenizer.advance()
            }

            // (

            tokenizer.advance()
            val nExpr = compileExpressionList()
            if (isVarCall) {
              writer.writeCall(name, nExpr + 1)
            } else {
              writer.writeCall(name, nExpr)
            }

            // )
            tokenizer.advance()
          } else if (tokenizer.symbol() == '[') {
            // varName '[' expression ']' (array access)

            val seg = symbolTable.kindOf(name) match {
              case StaticKind => StaticSegment
              case FieldKind => ThisSegment
              case ArgumentKind => ArgSegment
              case VarKind => LocalSegment
              case NoneKind => TempSegment
            }

            // '['

            tokenizer.advance()
            compileExpression()

            // ']'

            writer.writePush(seg, symbolTable.indexOf(name))
            writer.writeArithmetic(ADD)

            writer.writePop(PointerSegment, 1)
            writer.writePush(ThatSegment, 0)

            tokenizer.advance()
          } else { // varName
            val seg = symbolTable.kindOf(name) match {
              case StaticKind => StaticSegment
              case FieldKind => ThisSegment
              case ArgumentKind => ArgSegment
              case VarKind => LocalSegment
              case NoneKind => TempSegment
            }
            writer.writePush(seg, symbolTable.indexOf(name))
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
