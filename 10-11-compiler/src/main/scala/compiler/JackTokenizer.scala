package compiler

import java.io.{BufferedReader,FileReader}

sealed abstract class TokenType
case object KEYWORD extends TokenType
case object SYMBOL extends TokenType
case object IDENTIFIER extends TokenType
case object INT_CONST extends TokenType
case object STRING_CONST extends TokenType

sealed abstract class KW
case object CLASS extends KW
case object METHOD extends KW
case object FUNCTION extends KW
case object CONSTRUCTOR extends KW
case object INT extends KW
case object BOOLEAN extends KW
case object CHAR extends KW
case object VOID extends KW
case object VAR extends KW
case object STATIC extends KW
case object FIELD extends KW
case object LET extends KW
case object DO extends KW
case object IF extends KW
case object ELSE extends KW
case object WHILE extends KW
case object RETURN extends KW
case object TRUE extends KW
case object FALSE extends KW
case object NULL extends KW
case object THIS extends KW

class JackTokenizer(_filepath: String) {
  val keywords = Seq("class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return")
  val symbols = Seq("{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "|", "<", ">", "=", "~")

  val typeMatcher = Map(
    "keyword" -> keywords.mkString("|"),
    "symbol" -> { for (s <- symbols) yield "\\" + s }.mkString("|"),
    "integerConstant" -> "\\d+",
    "stringConstant" -> "\"[^\"\n]*\"",
    "identifier" -> "\\D\\w*"
  )

  // get all lines
  val fileReader = new BufferedReader(new FileReader(_filepath))
  val code = Iterator.continually(fileReader.readLine()).takeWhile(_ != null).mkString("\n")

  // normalize code and separate the code by whitespaces
  // (double-quoted string constants must be handled correctly)
  val tokens = { "([^\\s\"]+)|(\"[^\"]*\")".r.findAllIn(normalize(code)) }.toIterator

  var token = ""

  def normalize(s: String) = {
    // remove comments and trim the code
    val sCommentless = "(//.*?\n|/\\*[\\s\\S]*?\\*/)".r.replaceAllIn(s, "").trim

    // keep string constants
    val stringConstants = typeMatcher("stringConstant").r.findAllIn(sCommentless)

    // replace string constants with temporary string "STRING_CONST"
    val sTmpString = typeMatcher("stringConstant").r.replaceAllIn(sCommentless, "\"STRING\"")

    // add spaces to both ends of symbols
    var res = typeMatcher("symbol").r.replaceAllIn(sTmpString, " " + _ + " ")

    // put back the keeping string constants
    for (sc <- stringConstants) res = res.replaceFirst("\"STRING\"", sc)
    res
  }

  def hasMoreTokens(): Boolean = tokens.hasNext

  // called when hasMoreTokens() returned true
  def advance() = { token = tokens.next() }

  def tokenType(): TokenType = token match {
      case t if t.matches(typeMatcher("keyword"))          => KEYWORD
      case t if t.matches(typeMatcher("symbol"))           => SYMBOL
      case t if t.matches(typeMatcher("integerConstant"))  => INT_CONST
      case t if t.matches(typeMatcher("stringConstant"))   => STRING_CONST
      case t if t.matches(typeMatcher("identifier"))       => IDENTIFIER
    }

  def keyWord(): KW = token match {
      case "class"        => CLASS
      case "method"       => METHOD
      case "function"     => FUNCTION
      case "constructor"  => CONSTRUCTOR
      case "int"          => INT
      case "boolean"      => BOOLEAN
      case "char"         => CHAR
      case "void"         => VOID
      case "var"          => VAR
      case "static"       => STATIC
      case "field"        => FIELD
      case "let"          => LET
      case "do"           => DO
      case "if"           => IF
      case "else"         => ELSE
      case "while"        => WHILE
      case "return"       => RETURN
      case "true"         => TRUE
      case "false"        => FALSE
      case "null"         => NULL
      case "this"         => THIS
    }

  def symbol(): Char = token.charAt(0)

  def identifier(): String = token

  def intVal(): Int = token.toInt

  def stringVal(): String = token.replaceAll("\"", "")
}
