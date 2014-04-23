package elm

sealed case class Signal[T]()
sealed case class Element()
sealed case class Var[T](name: String) {
  override def toString() = Var.stringVal(this)
}

object Var {
  implicit def stringVal[A](v: Var[A]): String = v.name
}

// Expr AST stuff
sealed abstract class Expr[+A] {
  override def toString() = Expr.stringVal(this)
}

case class UnitE() extends Expr[Unit]
case class NumE(n: Int) extends Expr[Int]
case class StringE(s: String) extends Expr[String]
case class VarE[A](v: Var[A]) extends Expr[A]
case class Lam1E[A, B](v: Var[A], e: Expr[B])
    extends Expr[A => B]
case class App1E[A, B](e1: Expr[A => B], e2: Expr[A])
    extends Expr[B]
case class App2E[A, B, C](e1: Expr[(A, B) => C], e2: Expr[A],
                          e3: Expr[B]) extends Expr[C]
case class BinOpE(op: BinOp, e1: Expr[Int], e2: Expr[Int])
    extends Expr[Int]
case class InputSignalE[A](i: Int) extends Expr[Signal[A]]
case class LiftE1[A, B](e1: Expr[A => Signal[B]], e2: Expr[A])
    extends Expr[Signal[B]]
case class BuiltInE[A](v: Var[A]) extends Expr[A]
case class Tup2E[A, B](e1: Expr[A], e2: Expr[B])
    extends Expr[(A, B)]
case class Tup3E[A, B, C](e1: Expr[A], e2: Expr[B], e3: Expr[C])
    extends Expr[(A, B, C)]

object Expr {
  implicit def stringVal[A](e: Expr[A]): String = e match {
    case UnitE()            => "()"
    case NumE(n)            => n.toString
    case StringE(s)         => "\"" + s + "\""
    case VarE(v)            => v.toString
    case Lam1E(v, e)        => "function (" + v + ") { return " + e + "; }"
    case App1E(e1, e2)      => e1 + "(" + e2 + ")"
    case App2E(e1, e2, e3)  => appE(e1, e2, e3)
    case BinOpE(op, e1, e2) => e1 + op + e2
    case BuiltInE(v)        => v.toString
    case Tup2E(a, b)        => tup(a, b)
    case Tup3E(a, b, c)     => tup(a, b, c)
  }

  private def appE(e: Expr[Any], es: Expr[Any]*) = {
    "A" + es.size + "(" + e + ", " + es.mkString(", ") + ")"
  }

  private def tup(es: Expr[Any]*) = {
    var s = "{ ctor: \"_Tuple" + es.size + "\""
    for (i <- 0 to es.size-1) {
      s += ",_" + i + ": " + es(i) + " "
    }
    s += "}"
    s
  }

  implicit def expToFunc2[A,B,C](f: Expr[(A,B) => C]): Func2[A, B, C] = Func2(f)

  implicit def expToFunc1[A,B](f: Expr[A => B]): Func1[A, B] = Func1(f)
}

sealed case class Func1[A, B](expr: Expr[A => B]) {
  def apply(a: Expr[A]): Expr[B] = App1E(expr, a)
}

sealed case class Func2[A, B, C](expr: Expr[(A, B) => C]) {
  def apply(a: Expr[A], b: Expr[B]): Expr[C] = App2E(expr, a, b)
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
case class Func2T[A, B, C]() extends SimpleType[(A, B) => C]

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
