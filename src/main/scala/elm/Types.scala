package elm

sealed case class Signal[T]()
sealed case class Element()
sealed case class Variable[T](name: String) {
  override def toString() = Variable.stringVal(this)
}

object Variable {
  implicit def stringVal[A](v: Variable[A]): String = v.name
}

// Expr AST stuff
sealed abstract class Expr[+A] {
  override def toString() = Expr.stringVal(this)
}

case class UnitE() extends Expr[Unit]
case class NumE(n: Int) extends Expr[Int]
case class StringE(s: String) extends Expr[String]
case class VarE[A](v: Variable[A]) extends Expr[A]
case class LamE1[A, B](v: Variable[A], e: Expr[B])
    extends Expr[A => B]
case class AppE1[A, B](e1: Expr[A => B], e2: Expr[A])
    extends Expr[B]
case class AppE2[A, B, C](e1: Expr[(A, B) => C], e2: Expr[A],
                          e3: Expr[B]) extends Expr[C]
case class BinOpE(op: BinOp, e1: Expr[Int], e2: Expr[Int])
    extends Expr[Int]
case class InputSignalE[A](i: Int) extends Expr[Signal[A]]
case class LiftE1[A, B](e1: Expr[A => Signal[B]], e2: Expr[A])
    extends Expr[Signal[B]]
case class BuiltInE[A](v: Variable[A]) extends Expr[A]

object Expr {
  implicit def stringVal[A](e: Expr[A]): String = e match {
    case UnitE()            => "()"
    case NumE(n)            => n.toString
    case StringE(s)         => "\"" + s + "\""
    case VarE(v)            => v.toString
    case LamE1(v, e)        => "function (" + v + ") { return " + e + "; }"
    case AppE1(e1, e2)      => e1 + "(" + e2 + ")"
    case AppE2(e1, e2, e3)  => appE(e1, e2, e3)
    case BinOpE(op, e1, e2) => e1 + op + e2
    case BuiltInE(v)        => v.toString
  }

  private def appE[A](e: Expr[A], es: Expr[Any]*) = {
    "A" + es.size + "(" + e + ", " + es.mkString(", ") + ")"
  }

  implicit def expToFunc2[A,B,C](f: Expr[(A,B) => C]): Func2[A, B, C] = Func2(f)

  implicit def expToFunc1[A,B](f: Expr[A => B]): Func1[A, B] = Func1(f)
}

sealed case class Func1[A, B](expr: Expr[A => B]) {
  def apply(a: Expr[A]): Expr[B] = AppE1(expr, a)
}

sealed case class Func2[A, B, C](expr: Expr[(A, B) => C]) {
  def apply(a: Expr[A], b: Expr[B]): Expr[C] = AppE2(expr, a, b)
}

// Binary operators for type Expr
sealed abstract class BinOp {
  override def toString() = BinOp.stringVal(this)
}

case class AddB() extends BinOp
case class SubB() extends BinOp
case class MulB() extends BinOp
case class DivB() extends BinOp

object BinOp {
  implicit def stringVal(op: BinOp): String = op match {
    case AddB() => "+"
    case SubB() => "-"
    case MulB() => "*"
    case DivB() => "/"
  }
}

// SimpleType AST stuff
sealed abstract class SimpleType[T]
case class UnitT() extends SimpleType[Unit]
case class NumT() extends SimpleType[Int]
case class StringT() extends SimpleType[String]
case class ElementT() extends SimpleType[Element]
case class Func1T[A, B](a: SimpleType[A], b: SimpleType[B]) extends SimpleType[A => B]
case class Func2T[A, B, C]() extends SimpleType[A => B => C]

object SimpleType {
  implicit def simpleToType[A](s: SimpleType[A]): Type[A] = SimpleT(s)
}

// SignalType AST stuff
sealed abstract class SignalType[A]
case class SigT[A](t: SimpleType[A]) extends SignalType[A]
case class SimpToSigT[A, B](t1: SimpleType[A], t2: SignalType[B])
    extends SignalType[A => B]
case class SigToSigT[A, B](t1: SignalType[A], t2: SignalType[B])
    extends SignalType[A => B]

object SignalType {
  implicit def signalToType[A](s: SignalType[A]): Type[Signal[A]] =
    SignalT(s)
}

// Type AST stuff
sealed abstract class Type[+A]
case class SignalT[A](t: SignalType[A]) extends Type[Signal[A]]
case class SimpleT[A](t: SimpleType[A]) extends Type[A]

// Statement AST stuff
sealed abstract class Statement
case class InitS(id: String, e: Expr[_]) extends Statement
case class ImportS(path: String, open: Boolean = false) extends Statement

object Statement {
  implicit def stringVal(s: Statement): String = s match {
    case InitS(id, e) => "var " + id + " = " + e + ";\n"
    case ImportS(path, open) => "importS"
  }
}
