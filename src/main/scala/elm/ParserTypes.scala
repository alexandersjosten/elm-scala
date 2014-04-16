package elm

sealed abstract class ParserType {
  // Possibly add toString..
}
case class TupleType(ts: List[ParserType]) extends ParserType
case class VarP(s: String) extends ParserType
case class Fun(ts: List[ParserType]) extends ParserType

object Fun {
  def apply(t: ParserType*): Fun = Fun(t.toList)
}
