package jackanalizer

import java.io.{BufferedReader,FileReader,PrintWriter}
import jackanalizer._

class CompilationEngineXML(_in_filepath: String, _out_filepath: String) {
  val tokenizer = new JackTokenizer(_in_filepath)
  val writer = new PrintWriter(_out_filepath)

  // for each `compileXX` methods...
  // Rule #1: "advance" for the 1st grammer element is done by caller
  // Rule #2: each grammer finally does "advance" for the next unknown token
  if (tokenizer.hasMoreTokens()) {
    tokenizer.advance() // class
    if (tokenizer.tokenType() == KEYWORD &&
        tokenizer.keyWord() == CLASS) compileClass()
  }

  def printToken() {
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

  def compileClass() {
    writer.println("<class>")
    printToken() // class

    tokenizer.advance()
    printToken() // className

    tokenizer.advance()
    printToken() // {

    tokenizer.advance()

    while (tokenizer.tokenType() == KEYWORD &&
           Seq(STATIC, FIELD).contains(tokenizer.keyWord())) {
      compileClassVarDec()
    }

    while (tokenizer.tokenType() == KEYWORD &&
           Seq(CONSTRUCTOR, FUNCTION, METHOD).contains(tokenizer.keyWord())) {
      compileSubroutine()
    }

    printToken() // }

    writer.println("</class>")
    writer.close
  }

  def compileClassVarDec() {
    writer.println("<classVarDec>")

    printToken() // 'static' | 'field'

    tokenizer.advance()
    printToken() // type

    tokenizer.advance()
    printToken() // varName

    tokenizer.advance()
    while (tokenizer.tokenType() == SYMBOL &&
           tokenizer.symbol() == ',') {
      printToken() // ,

      tokenizer.advance()
      printToken() // varName

      tokenizer.advance()
    }

    printToken() // ;

    writer.println("</classVarDec>")

    tokenizer.advance()
  }

  def compileSubroutine() {
    writer.println("<subroutineDec>")

    printToken() // 'constructor' | 'function' | 'method'

    tokenizer.advance()
    printToken() // 'void' | type

    tokenizer.advance()
    printToken() // subroutineName

    tokenizer.advance()
    printToken() // (

    tokenizer.advance()
    compileParameterList()

    printToken() // )

    writer.println("<subroutineBody>")

    tokenizer.advance()
    printToken() // {

    tokenizer.advance()
    while (tokenizer.tokenType() == KEYWORD &&
           tokenizer.keyWord() == VAR) compileVarDec()

    compileStatements()

    printToken() // }

    writer.println("</subroutineBody>")

    writer.println("</subroutineDec>")

    tokenizer.advance()
  }

  def compileParameterList() {
    writer.println("<parameterList>")

    val type_keywords = Seq(INT, CHAR, BOOLEAN)
    if (tokenizer.tokenType() == IDENTIFIER ||
        (tokenizer.tokenType() == KEYWORD &&
         type_keywords.contains(tokenizer.keyWord()))) {
      printToken() // type

      tokenizer.advance()
      printToken() // varName

      tokenizer.advance()
      while (tokenizer.tokenType() == SYMBOL &&
             tokenizer.symbol() == ',') {
        printToken() // ,

        tokenizer.advance()
        printToken() // type

        tokenizer.advance()
        printToken() // varName

        tokenizer.advance()
      }
    }

    writer.println("</parameterList>")
  }

  def compileVarDec() {
    writer.println("<varDec>")

    printToken() // 'var'

    tokenizer.advance()
    printToken() // type

    tokenizer.advance()
    printToken() // varName

    tokenizer.advance()
    while (tokenizer.tokenType() == SYMBOL &&
           tokenizer.symbol() == ',') {
      printToken() // ,

      tokenizer.advance()
      printToken() // varName

      tokenizer.advance()
    }

    printToken() // ;

    writer.println("</varDec>")

    tokenizer.advance()
  }

  def compileStatements() {
    writer.println("<statements>")

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

    writer.println("</statements>")
  }

  def compileDo() {
    writer.println("<doStatement>")

    printToken() // 'do'

    tokenizer.advance()
    compileSubroutineCall()

    printToken() // ;

    writer.println("</doStatement>")

    tokenizer.advance()
  }

  def compileSubroutineCall() {
    printToken() // subroutineName | className | varName

    tokenizer.advance()
    if (tokenizer.symbol() == '.') {
      printToken() // .

      tokenizer.advance()
      printToken() // subroutineName

      tokenizer.advance()
    }

    printToken() // (

    tokenizer.advance()
    compileExpressionList()

    printToken() // )

    tokenizer.advance()
  }

  def compileLet() {
    writer.println("<letStatement>")

    printToken() // 'let'

    tokenizer.advance()
    printToken() // varName

    tokenizer.advance()
    if (tokenizer.tokenType() == SYMBOL &&
        tokenizer.symbol() == '[') {
      printToken() // '['

      tokenizer.advance()
      compileExpression()

      printToken() // ']'

      tokenizer.advance()
    }

    printToken() // '='

    tokenizer.advance()
    compileExpression()

    printToken() // ';'

    writer.println("</letStatement>")

    tokenizer.advance()
  }

  def compileWhile() {
    writer.println("<whileStatement>")

    printToken() // 'while'

    tokenizer.advance()
    printToken() // '('

    tokenizer.advance()
    compileExpression()

    printToken() // ')'

    tokenizer.advance()
    printToken() // '{'

    tokenizer.advance()
    compileStatements()

    printToken() // '}'

    writer.println("</whileStatement>")

    tokenizer.advance()
  }

  def compileReturn() {
    writer.println("<returnStatement>")

    printToken() // 'return'

    tokenizer.advance()
    if (tokenizer.tokenType() != SYMBOL ||
        (tokenizer.tokenType() == SYMBOL &&
         tokenizer.symbol() != ';')) compileExpression()

    printToken() // ';'

    writer.println("</returnStatement>")

    tokenizer.advance()
  }

  def compileIf() {
    writer.println("<ifStatement>")

    printToken() // 'if'

    tokenizer.advance()
    printToken() // '('

    tokenizer.advance()
    compileExpression()

    printToken() // ')'

    tokenizer.advance()
    printToken() // '{'

    tokenizer.advance()
    compileStatements()

    printToken() // '}'

    writer.println("</ifStatement>")

    tokenizer.advance()
  }

  def compileExpression() {
    writer.println("<expression>")

    compileTerm()

    val op = Seq('+', '-', '*', '/', '&', '|', '<', '>', '=')
    while (tokenizer.tokenType() == SYMBOL &&
           op.contains(tokenizer.symbol())) {
      printToken() // op

      tokenizer.advance()
      compileTerm()
    }

    writer.println("</expression>")
  }

  def compileTerm() {
    writer.println("<term>")

    tokenizer.tokenType() match {
      case SYMBOL => {
        tokenizer.symbol() match {
          case '('       => printToken() // '('
                            tokenizer.advance()
                            compileExpression()
                            printToken() // ')'
                            tokenizer.advance()
          case '-' | '~' => printToken() // '-' | '~'
                            tokenizer.advance()
                            compileTerm()
        }
      }
      case KEYWORD => {
        val keyWordConstant = Seq(TRUE, FALSE, NULL, THIS)
        if (keyWordConstant.contains(tokenizer.keyWord())) {
          printToken()
          tokenizer.advance()
        }
      }
      case STRING_CONST | INT_CONST => {
        printToken()
        tokenizer.advance()
      }
      case IDENTIFIER => {
        printToken() // varName | subroutineName
        tokenizer.advance()

        if (tokenizer.tokenType() == SYMBOL) {
          if (Seq('.', '(').contains(tokenizer.symbol())) {
            // subroutineCall

            if (tokenizer.symbol() == '.') {
              printToken() // .

              tokenizer.advance()
              printToken() // subroutineName

              tokenizer.advance()
            }

            printToken() // (

            tokenizer.advance()
            compileExpressionList()

            printToken() // )
            tokenizer.advance()
          } else if (tokenizer.symbol() == '[') {
            // varName '[' expression ']'

            printToken() // '['

            tokenizer.advance()
            compileExpression()

            printToken() // ']'
            tokenizer.advance()
          }
        }
      }
    }

    writer.println("</term>")
  }

  def compileExpressionList() {
    writer.println("<expressionList>")

    // non-empty expressionList
    if (tokenizer.tokenType() != SYMBOL ||
        (tokenizer.tokenType() == SYMBOL &&
         tokenizer.symbol() != ')')) {

      compileExpression()

      while (tokenizer.tokenType() == SYMBOL &&
             tokenizer.symbol() == ',') {
        printToken() // ','

        tokenizer.advance()
        compileExpression()
      }
    }

    writer.println("</expressionList>")
  }
}
