package elm

sealed abstract class ParserType {
  implicit def stringVal(pt: ParserType): String = {
    pt match {
      case VarP(s)       => s
      case TupleType(ts) => "(" + ts.mkString(",") + ")"
      case Fun(ts)       =>
        var str: String = "Expr["
        if(ts.length > 2) str += "("
        str += ts.init.mkString(", ")
        if(ts.length > 2) str += ")"
        str += "] => Expr[" + ts.last.toString + "]"
        str
    }
  }

  override def toString: String = stringVal(this)
}

case class VarP(s: String) extends ParserType
case class TupleType(ts: List[ParserType]) extends ParserType
case class Fun(ts: List[ParserType]) extends ParserType

object Fun {
  def apply(t: ParserType*): Fun = Fun(t.toList)
}
