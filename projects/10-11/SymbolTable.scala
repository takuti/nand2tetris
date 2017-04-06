package jackanalizer

sealed abstract class Kind
case object STATIC extends Kind
case object FIELD extends Kind
case object ARGUMENT extends Kind
case object VAR extends Kind
case object NONE extends Kind

class Symbol(stype: String, kind: Kind, idx: Int)

class SymbolTable {
  val stClass = Map()
  val stSubroutine = Map()
  val counter = Map(STATIC -> 0, FIELD -> 0, ARGUMENT -> 0, VAR -> 0)

  def startSubroutine = {
    stSubroutine.clear()
    counter(ARGUMENT) = 0
    counter(VAR) = 0
  }

  def define(name: String, stype: String, kind: Kind) = {
    if (kind == STATIC || kind == FIELD)
      stClass.put(name, new Symbol(stype, kind, counter(kind)))
    else
      stSubroutine.put(name, new Symbol(stype, kind, counter(kind)))
    counter(kind) += 1
  }

  def varCount(kind: Kind): Int = {
    if (kind == STATIC || kind == FIELD)
      stClass.filter(t => t._2.kind == kind).length
    else
      stSubroutine.filter(t => t._2.kind == kind).length
  }

  def kindOf(name: String): Kind = {
    if (stClass.contains(name)) stClass(name).kind
    else if (stSubroutine.contains(name)) stSubroutine(name).kind
    else NONE
  }

  def typeOf(name: String): String = {
    if (stClass.contains(name)) stClass(name).stype
    else if (stSubroutine.contains(name)) stSubroutine(name).stype
  }

  def indexOf(name: String): Int = {
    if (stClass.contains(name)) stClass(name).idx
    else if (stSubroutine.contains(name)) stSubroutine(name).idx
  }
}
