package compiler

sealed abstract class Kind
case object StaticKind extends Kind
case object FieldKind extends Kind
case object ArgumentKind extends Kind
case object VarKind extends Kind
case object NoneKind extends Kind

class SymbolTable {
  case class Symbol(val stype: String, val kind: Kind, val idx: Int)

  val stClass = scala.collection.mutable.Map[String, Symbol]()
  val stSubroutine = scala.collection.mutable.Map[String, Symbol]()
  val counter = scala.collection.mutable.Map(
    StaticKind.toString -> 0, FieldKind.toString -> 0,
    ArgumentKind.toString -> 0, VarKind.toString -> 0)

  def startSubroutine() = {
    stSubroutine.clear()
    counter(ArgumentKind.toString) = 0
    counter(VarKind.toString) = 0
  }

  def define(name: String, stype: String, kind: Kind) = {
    if (kind == StaticKind || kind == FieldKind)
      stClass.put(name, new Symbol(stype, kind, counter(kind.toString)))
    else
      stSubroutine.put(name, new Symbol(stype, kind, counter(kind.toString)))
    counter(kind.toString) += 1
  }

  def varCount(kind: Kind): Int =
    if (kind == StaticKind || kind == FieldKind)
      stClass.filter(t => t._2.kind == kind).size
    else
      stSubroutine.filter(t => t._2.kind == kind).size

  def kindOf(name: String): Kind =
    if (stClass.contains(name)) stClass(name).kind
    else if (stSubroutine.contains(name)) stSubroutine(name).kind
    else NoneKind

  def typeOf(name: String): String =
    if (stClass.contains(name)) stClass(name).stype
    else if (stSubroutine.contains(name)) stSubroutine(name).stype
    else ""

  def indexOf(name: String): Int =
    if (stClass.contains(name)) stClass(name).idx
    else if (stSubroutine.contains(name)) stSubroutine(name).idx
    else 0
}
