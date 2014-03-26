package elm

// Expression AST stuff
sealed abstract class Expression {
  override def toString() = Expression.stringVal(this)
}

case class UnitE() extends Expression
case class NumE(n : Int) extends Expression
case class VarE(v : String) extends Expression
case class LambdaE(v : String, t : Type, e : Expression) extends Expression
case class AppE(e1 : Expression, e2 : Expression) extends Expression
case class BinOpE(b : BinOp, e1 : Expression, e2 : Expression) extends Expression
case class IfE(e1 : Expression, e2 : Expression, e3 : Expression) extends Expression
case class InputSignalE(i : Int) extends Expression
case class LiftE(e : Expression, es : List[Expression]) extends Expression
case class FoldpE(e1 : Expression, e2 : Expression, e3 : Expression) extends Expression
case class AsyncE(e : Expression) extends Expression  // Will not be supported..

object Expression {
  implicit def stringVal(e : Expression) : String = e match {
    case NumE(n) => n.toString
  }
}

// Binary operators for type Expression
sealed abstract class BinOp
case class AddB() extends BinOp
case class SubB() extends BinOp
case class MulB() extends BinOp
case class DivB() extends BinOp

// SimpleType AST stuff
sealed abstract class SimpleType
case class UnitT() extends SimpleType
case class IntT() extends SimpleType
case class FuncT(t1 : SimpleType, t2 : SimpleType) extends SimpleType

// SignalType AST stuff
sealed abstract class SignalType
case class SigT(t : SimpleType) extends SignalType
case class SimpToSigT(t1 : SimpleType, t2 : SignalType) extends SignalType
case class SigToSigT(t1 : SignalType, t2 : SignalType) extends SignalType

// Type AST stuff
sealed abstract class Type
case class SignalT(t : SignalType) extends Type
case class SimpleT(t : SimpleType) extends Type

// Statement AST stuff
sealed abstract class Statement
case class InitS(id : String, e : Expression) extends Statement
case class ImportS(path : String, open : Boolean = false) extends Statement

object Statement {
  implicit def stringVal(s : Statement) : String = s match {
    case InitS(id, e) => "var " + id + " = " + e + ";\n"
    case ImportS(path, open) => "importS"
  }
}
