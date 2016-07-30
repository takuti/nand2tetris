package jackanalizer

import java.io.{BufferedReader,FileReader}

sealed abstract class TokenType
case object KEYWORD extends TokenType
case object SYMBOL extends TokenType
case object IDENTIFIER extends TokenType
case object INT_CONST extends TokenType
case object STRING_CONST extends TokenType

class JackTokenizer(_filepath: String) {
  val keywords = Array("class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return")
  val symbols = Array("{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "|", "<", ">", "=", "~")

  val type_matcher = Map(
    "keyword" -> keywords.mkString("|"),
    "symbol" -> { for (s <- symbols) yield "\\" + s }.mkString("|"),
    "integerConstant" -> "\\d+",
    "stringConstant" -> "\"[^\"\n]*\"",
    "identifier" -> "\\D\\w*"
  )

  // get all lines
  val file_reader = new BufferedReader(new FileReader(_filepath))
  val code = Iterator.continually(file_reader.readLine()).takeWhile(_ != null).mkString("\n")

  // normalize code and separate the code by whitespaces
  // (double-quoted string constants must be handled correctly)
  val tokens = { "([^\\s\"]+)|(\"[^\"]*\")".r.findAllIn(normalize(code)) }.toIterator

  var token = ""

  def normalize(s: String) = {
    // remove comments and trim the code
    val s_commentless = "(//.*?\n|/\\*[\\s\\S]*?\\*/)".r.replaceAllIn(s, "").trim

    // keep string constants
    val string_constants = type_matcher("stringConstant").r.findAllIn(s_commentless)

    // replace string constants with temporary string "STRING_CONST"
    val s_tmp_string = type_matcher("stringConstant").r.replaceAllIn(s_commentless, "\"STRING\"")

    // add spaces to both ends of symbols
    var res = type_matcher("symbol").r.replaceAllIn(s_tmp_string, " " + _ + " ")

    // put back the keeping string constants
    for (sc <- string_constants) res = res.replaceFirst("\"STRING\"", sc)
    res
  }

  def hasMoreTokens(): Boolean = tokens.hasNext

  // called when hasMoreTokens() returned true
  def advance() = { token = tokens.next() }

  def tokenType() = {
    token match {
      case t if t.matches(type_matcher("keyword"))          => KEYWORD
      case t if t.matches(type_matcher("symbol"))           => SYMBOL
      case t if t.matches(type_matcher("integerConstant"))  => INT_CONST
      case t if t.matches(type_matcher("stringConstant"))   => STRING_CONST
      case t if t.matches(type_matcher("identifier"))       => IDENTIFIER
    }
  }
}
