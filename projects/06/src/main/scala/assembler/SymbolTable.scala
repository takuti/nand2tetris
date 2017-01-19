package assembler

class SymbolTable {
  val table = collection.mutable.Map[String,Int]()

  def addEntry(symbol: String, address: Int) = table put (symbol, address)

  def contains(symbol: String): Boolean = table contains symbol

  def getAddress(symbol: String): Int = table(symbol)
}
