sealed abstract class Expression
case class UnitE() extends Expression
case class NumE(n : Int) extends Expression
case class VarE(v : String) extends Expression
case class LambdaE(v : String, t : Type, e : Expression) extends Expression


sealed abstract class Type
