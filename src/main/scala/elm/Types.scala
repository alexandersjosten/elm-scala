package elm

sealed case class Signal[T]()
sealed case class Element()
sealed case class Variable[T](name: String) {
  override def toString() = Variable.stringVal(this)
}

object Variable {
  implicit def stringVal[A](v: Variable[A]): String = v.name
}

// Expression AST stuff
sealed abstract class Expression[+T] {
  override def toString() = Expression.stringVal(this)
}

case class UnitE() extends Expression[Unit]
case class NumE(n: Int) extends Expression[Int]
case class StringE(s: String) extends Expression[String]
case class VarE[T](v: Variable[T]) extends Expression[T]
case class LambdaE[T1, T2](v: Variable[T1], e: Expression[T2])
    extends Expression[T1 => T2]
case class AppE1[T1, T2](e1: Expression[T1 => T2], e2: Expression[T1])
    extends Expression[T2]
case class AppE2[T1, T2, T3](e1: Expression[(T1, T2) => T3], e2: Expression[T1],
                             e3: Expression[T2]) extends Expression[T3]
case class BinOpE(op: BinOp, e1: Expression[Int], e2: Expression[Int])
    extends Expression[Int]
case class InputSignalE[T](i: Int) extends Expression[Signal[T]]
case class LiftE1[T1, T2](e1: Expression[T1 => Signal[T2]], e2: Expression[T1])
    extends Expression[Signal[T2]]
case class BuiltInE[T](v: Variable[T]) extends Expression[T]

object Expression {
  implicit def stringVal[A](e: Expression[A]): String = e match {
    case UnitE()            => "()"
    case NumE(n)            => n.toString
    case StringE(s)         => "\"" + s + "\""
    case VarE(v)            => v.toString
    case LambdaE(v, e)      => "function (" + v + ") { return " + e + "; }"
    case AppE1(e1, e2)      => e1 + "(" + e2 + ")"
    case AppE2(e1, e2, e3)  => appE(e1, e2, e3)
    case BinOpE(op, e1, e2) => e1 + op + e2
    case BuiltInE(v)        => v.toString
  }

  private def appE[A](e: Expression[A], es: Expression[Any]*) = {
    "A" + es.size + "(" + e + ", " + es.mkString(", ") + ")"
  }

  implicit def expToFunc2[A,B,C](f: Expression[(A,B) => C]): Func2[A, B, C] = Func2(f)

  implicit def expToFunc1[A,B](f: Expression[A => B]): Func1[A, B] = Func1(f)
}

sealed case class Func1[A, B](expr: Expression[A => B]) {
  def apply(a: Expression[A]): Expression[B] = AppE1(expr, a)
}

sealed case class Func2[A, B, C](expr: Expression[(A, B) => C]) {
  def apply(a: Expression[A], b: Expression[B]): Expression[C] = AppE2(expr, a, b)
}

// Binary operators for type Expression
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
  implicit def simpleToType[T](s: SimpleType[T]): Type[T] = SimpleT(s)
}

// SignalType AST stuff
sealed abstract class SignalType[T]
case class SigT[T](t: SimpleType[T]) extends SignalType[T]
case class SimpToSigT[T1, T2](t1: SimpleType[T1], t2: SignalType[T2])
    extends SignalType[T1 => T2]
case class SigToSigT[T1, T2](t1: SignalType[T1], t2: SignalType[T2])
    extends SignalType[T1 => T2]

object SignalType {
  implicit def signalToType[T](s: SignalType[T]): Type[Signal[T]] =
    SignalT(s)
}

// Type AST stuff
sealed abstract class Type[T]
case class SignalT[T](t: SignalType[T]) extends Type[Signal[T]]
case class SimpleT[T](t: SimpleType[T]) extends Type[T]

// Statement AST stuff
sealed abstract class Statement
case class InitS(id: String, e: Expression[_]) extends Statement
case class ImportS(path: String, open: Boolean = false) extends Statement

object Statement {
  implicit def stringVal(s: Statement): String = s match {
    case InitS(id, e) => "var " + id + " = " + e + ";\n"
    case ImportS(path, open) => "importS"
  }
}
