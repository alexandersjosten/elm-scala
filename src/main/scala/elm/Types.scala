package elm

sealed case class Signal[T]()
sealed case class Element()
sealed case class Var[T](name: String)

object Var {
  implicit def toJs(v: Var[Any]) = JsName(v.name)
}

// Expr AST stuff
sealed abstract class Expr[+A] {
  def toJs() = Expr.toJs(this)
}

case class UnitE() extends Expr[Unit]
case class NumE(n: Int) extends Expr[Int]
case class StringE(s: String) extends Expr[String]
case class VarE[A](v: Var[A]) extends Expr[A]
case class Lam1E[A, B](v: Var[A], e: Expr[B])
    extends Expr[A => B]
case class Lam2E[A, B, C](v1: Var[A], v2: Var[B], e: Expr[C])
    extends Expr[(A, B) => C]
case class Lam3E[A, B, C, D](v1: Var[A], v2: Var[B], v3: Var[C], e: Expr[D])
    extends Expr[(A, B, C) => D]
case class App1E[A, B](e1: Expr[A => B], e2: Expr[A])
    extends Expr[B]
case class App2E[A, B, C](e1: Expr[(A, B) => C], e2: Expr[A],
                          e3: Expr[B]) extends Expr[C]
case class App3E[A, B, C, D](f: Expr[(A, B, C) => D], e1: Expr[A],
                          e2: Expr[B], e3: Expr[C]) extends Expr[D]
case class BinOpE(op: BinOp, e1: Expr[Int], e2: Expr[Int])
    extends Expr[Int]
case class BuiltInE[A](v: Var[A]) extends Expr[A]
case class Tup2E[A, B](e1: Expr[A], e2: Expr[B])
    extends Expr[(A, B)]
case class Tup3E[A, B, C](e1: Expr[A], e2: Expr[B], e3: Expr[C])
    extends Expr[(A, B, C)]
case class ListE[A](e1: List[Expr[A]])
    extends Expr[List[A]]

object Expr {
  implicit def toJs(e: Expr[Any]): JsExpr = e match {
    case UnitE() => JsUnit()
    case NumE(n) => JsNum(n)
    case StringE(s) => JsStr(s)
    case VarE(Var(n)) => JsName(n)
    case Lam1E(v, e) => JsAnonymousFunction(List(v), JsReturn(Some(e)))
    case Lam2E(v1, v2, e) => funExpr(e, v1, v2)
    case Lam3E(v1, v2, v3, e) => funExpr(e, v1, v2, v3)
    case App1E(f, e) => JsFunctionCall(f, e)
    case App2E(f, e1, e2) => appExpr(f, e1, e2)
    case App3E(f, e1, e2, e3) => appExpr(f, e1, e2, e3)
    case BinOpE(op, e1, e2) => JsBinOp(e1, op, e2)
    case BuiltInE(v) => v
    case Tup2E(e1, e2) => tupExpr(e1, e2)
    case ListE(ls) => JsFunctionCall(JsName("_J.toList"), JsArray(ls map toJs))
    case x => JsName(x.toString)
  }

  private def funExpr(body: Expr[Any], args: Var[Any]*) = {
    import scala.collection.mutable.ListBuffer

    val fun = JsName("F" + args.size)
    val args_ = args map { x => JsName(x.name) }
    val body_ = new JsReturn(JsFunctionCall(JsAnonymousFunction(List(), new JsReturn(body.toJs))))

    JsFunctionCall(fun, JsAnonymousFunction(args_.toList, body_))
  }

  private def tupExpr(es: Expr[Any]*) = {
    import scala.collection.mutable.ListBuffer
    val args: ListBuffer[(JsName, JsExpr)] = ListBuffer()
    args += ((JsName("ctor"), JsStr("_Tuple" + es.size)))

    var n: Int = -1
    es foreach { e =>
      n += 1
      args += ((JsName("_" + n), e))
    }

    JsObject( args.result )
  }

  private def appExpr(e: Expr[Any], es: Expr[Any]*) = {
    import scala.collection.mutable.ListBuffer
    val args: ListBuffer[JsExpr] = ListBuffer()
    args += e
    es foreach (args += _)

    JsFunctionCall("A" + es.size, args:_*)
  }

  implicit def expToFunc3[A,B,C,D](f: Expr[(A,B,C) => D]): Func3[A, B, C, D] = Func3(f)

  implicit def expToFunc2[A,B,C](f: Expr[(A,B) => C]): Func2[A, B, C] = Func2(f)

  implicit def expToFunc1[A,B](f: Expr[A => B]): Func1[A, B] = Func1(f)
}

sealed case class Func1[A, B](expr: Expr[A => B]) {
  def apply(a: Expr[A]): Expr[B] = App1E(expr, a)
}

sealed case class Func2[A, B, C](expr: Expr[(A, B) => C]) {
  def apply(a: Expr[A], b: Expr[B]): Expr[C] = App2E(expr, a, b)
}

sealed case class Func3[A, B, C, D](expr: Expr[(A, B, C) => D]) {
  def apply(a: Expr[A], b: Expr[B], c: Expr[C]): Expr[D] = App3E(expr, a, b, c)
}

// Binary operators for type Expr
sealed abstract class BinOp
case class AddB() extends BinOp
case class SubB() extends BinOp
case class MulB() extends BinOp
case class DivB() extends BinOp

object BinOp {
  implicit def toJs(b: BinOp): JsName = JsName(b match {
    case AddB() => "+"
    case SubB() => "-"
    case MulB() => "*"
    case DivB() => "/"
  })
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
