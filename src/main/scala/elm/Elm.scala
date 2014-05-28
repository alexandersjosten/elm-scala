package elm

import scala.collection.mutable.MutableList
import scala.collection.mutable.StringBuilder

class Program {
  private var mapping: Map[Int, Expr[Any]] = Map[Int, Expr[Any]]()
  private var varId: Int = -1
  private var lamId: Int = -1

  def addExpr[A](expr: Expr[A]): Expr[A] = {
    val id = getVar
    mapping += id -> expr
    VarE(Var(getName(id)))
  }

  def emit: String = {
    val str = new StringBuilder
    str ++= Constants.ElmHeader

    val elmJs: ElmJs = new ElmJs()
    val prog = genProgram

    prog.foreach {
      case InitS(n, e) =>
        elmJs.addDef(n, e)
      case _ => ()
    }

    str ++= elmJs.emit

    str ++= "\n" + Constants.ElmFooter
    str.toString
  }

  def getVar: Int = { varId += 1; varId }

  def getLam: String = { lamId += 1; "lam_" + lamId }

  private def indent(depth: Int, s: String): String =
    " " * depth + s

  private def genProgram: List[Statement] = {
    val stmts = MutableList[Statement]()
    val program = mapping.init

    program.foreach {
      case (n, expr) =>
        val name = getName(n)
        stmts += InitS(name, expr)
    }

    stmts += InitS("main", mapping.last._2)

    stmts.toList
  }

  private def getName(id: Int): String = "x_" + id

}

class Elm {
  val prog: Program = new Program()

  def __newVar[A](expr: Expr[A]): Expr[A] = prog.addExpr(expr)

  def __newVar[A, B](fun: Expr[A] => Expr[B]): Expr[A => B] = {
    val id = prog.getLam
    val x: Var[A] = Var(id)
    val body: Expr[B] = fun(VarE(x))
    val expr = Lam1E(x, body)

    prog.addExpr(expr)
  }

  def __newVar[A, B, C](fun: (Expr[A], Expr[B]) => Expr[C]): Expr[(A, B) => C] = {
    val idx = prog.getLam
    val idy = prog.getLam
    val x: Var[A] = Var(idx)
    val y: Var[B] = Var(idy)
    val body: Expr[C] = fun(VarE(x), VarE(y))
    val expr = Lam2E(x, y, body)

    prog.addExpr(expr)
  }

  def __newVar[A, B, C, D](fun: (Expr[A], Expr[B], Expr[C]) => Expr[D]): Expr[(A, B, C) => D] = {
    val id1 = prog.getLam
    val id2 = prog.getLam
    val id3 = prog.getLam
    val x: Var[A] = Var(id1)
    val y: Var[B] = Var(id2)
    val z: Var[C] = Var(id3)
    val body: Expr[D] = fun(VarE(x), VarE(y), VarE(z))
    val expr = Lam3E(x, y, z, body)

    prog.addExpr(expr)
  }


  def infix_+(a: Expr[Int], b: Expr[Int]) = BinOpE(AddB(), a, b)
  def infix_-(a: Expr[Int], b: Expr[Int]) = BinOpE(SubB(), a, b)
  def infix_*(a: Expr[Int], b: Expr[Int]) = BinOpE(MulB(), a, b)
  def infix_/(a: Expr[Int], b: Expr[Int]) = BinOpE(DivB(), a, b)

  implicit def expToExp[A](e: Expr[A]): Expr[A] = e
  implicit def unitToExp(e: Unit): Expr[Unit] = UnitE()
  implicit def intToExp(i: Int): Expr[Int] = NumE(i)
  implicit def stringToExp(s: String): Expr[String] = StringE(s)
  implicit def listToExp[A](ls: List[A])
    (implicit fa: A => Expr[A])
      : Expr[List[A]] = ListE(ls map fa)

  implicit def tup2ToExp1[A,B](p: (A, B))
    (implicit fa: A => Expr[A], fb: B => Expr[B])
      : Expr[(A, B)] = Tup2E(p._1, p._2)

  implicit def tup2ToExp2[A,B](p: (Expr[A], B))
    (implicit fb: B => Expr[B])
      : Expr[(A, B)] = Tup2E(p._1, p._2)

  implicit def tup2ToExp3[A,B](p: (A, Expr[B]))
    (implicit fa: A => Expr[A])
      : Expr[(A, B)] = Tup2E(p._1, p._2)

  implicit def tup2ToExp4[A,B](p: (Expr[A], Expr[B]))
      : Expr[(A, B)] = Tup2E(p._1, p._2)

  implicit def tup3ToExp[A,B,C](p: (A, B, C))
    (implicit fa: A => Expr[A], fb: B => Expr[B], fc: C => Expr[C])
      : Expr[(A, B, C)] = Tup3E(p._1, p._2, p._3)

}

class ElmJs {
  import scala.collection.mutable.MutableList

  private val defs: MutableList[(String, JsExpr)] =
    new MutableList()

  defs += (("_op", JsObject()))

  def addDef[A](n: String, e: JsExpr): Unit = defs += ((n, e))

  def emit: String = {
    val elmMain = JsAssignment("Elm.Main", JsBinOp("Elm.Main", "||", JsObject()))

    val makeBody = {
      JsStr("use strict") &
      JsAssignment("_elm.Main", JsBinOp("_elm.Main", "||", JsObject())) &
      JsIf("_elm.Main.values", new JsReturn("_elm.Main.values")) &
      JsDef("_N", "Elm.Native") &
      JsDef("_U", "_N.Utils.make(_elm)") &
      JsDef("_L", "_N.List.make(_elm)") &
      JsDef("_E", "_N.Error.make(_elm)") &
      JsDef("_J", "_N.JavaScript.make(_elm)") &
      JsDef("$moduleName", JsStr("Main")) &
      JsDef("Basics", "Elm.Basics.make(_elm)") &
      JsDef("Color", "Elm.Color.make(_elm)") &
      JsDef("Graphics", JsBinOp("Graphics", "||", JsObject())) &
      JsAssignment("Graphics.Collage", JsFunctionCall("Elm.Graphics.Collage.make", "_elm")) &
      JsDef("Graphics", JsBinOp("Graphics", "||", JsObject())) &
      JsAssignment("Graphics.Element", JsFunctionCall("Elm.Graphics.Element.make", "_elm")) &
      JsDef("List", JsFunctionCall("Elm.List.make", "_elm")) &
      JsDef("Maybe", JsFunctionCall("Elm.Maybe.make", "_elm")) &
      JsDef("Mouse", JsFunctionCall("Elm.Mouse.make", "_elm")) &
      JsDef("Native", JsBinOp("Native", "||", JsObject())) &
      JsAssignment("Native.Ports", JsFunctionCall("Elm.Native.Ports.make", "_elm")) &
      JsDef("Signal", JsFunctionCall("Elm.Signal.make", "_elm")) &
      JsDef("String", JsFunctionCall("Elm.String.make", "_elm")) &
      JsDef("Text", JsFunctionCall("Elm.Text.make", "_elm")) &
      JsDef("Time", JsFunctionCall("Elm.Time.make", "_elm")) &
      jsDefs &
      new JsReturn("_elm.Main.values")
    }

    val makeFun = JsAssignment("Elm.Main.make",
      JsAnonymousFunction(List("_elm"), makeBody))

    JsPrettyPrinter.prettyStmt(elmMain & makeFun)
  }

  private def jsDefs: JsStmt = {
    def toDef(x: (String, JsExpr)): JsStmt = JsDef(x._1, x._2)
    def toTup(x: (String, JsExpr)): (JsName, JsExpr) = (x._1, x._1)

    defs.tail.foldLeft(toDef(defs.head))(_ & toDef(_)) &
    JsAssignment("_elm.Main.values", JsObject(defs.map(toTup).toList))
  }

}
