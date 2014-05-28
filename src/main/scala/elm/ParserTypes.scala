package elm

sealed abstract class ParserType {
  def stringVal(pt: ParserType): String = "Expr[" + stringValAux(pt) + "]"

  def stringValAux(pt: ParserType): String = {
    pt match {
      case VarP(s)       =>
        if(s contains " ") {
          val index = s.indexOf(" ")
          s.substring(0, index) + "[" + s.substring(index + 1).toUpperCase.trim + "]"
        }
        else {
          s.capitalize
        }
      case TupleType(ts) => "(" + ts.mkString(",") + ")"
      case Fun(ts)       => {
        var str = ""
        if(ts.length > 2) str += "("
        str += ts.init.mkString(",")
        if(ts.length > 2) str += ")"
        str += " => " + ts.last.toString
        str
      }
      case App(l, r)     => l.toString + "[" + r.toString + "]"
    }
  }

  override def toString: String = stringValAux(this)
}

case class VarP(s: String) extends ParserType
case class TupleType(ts: List[ParserType]) extends ParserType
case class Fun(ts: List[ParserType]) extends ParserType
case class App(left: ParserType, right: ParserType) extends ParserType

object Fun {
  def apply(t: ParserType*): Fun = Fun(t.toList)
}
