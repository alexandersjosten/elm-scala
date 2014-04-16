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
    str ++= indent(3, "var _op = {};\n")

    val prog = genProgram

    prog.foreach { str ++= indent(3, _) }

    str ++= indent(3, "_elm.Main.values = {_op: _op")

    prog.foreach {
      case InitS(n, _) =>
        str ++= "\n" + indent(22, "," + n + ": " + n)
      case ImportS(_, _) => ()
    }
    str ++= "};\n" + Constants.ElmFooter
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

  def infix_+(a: Expr[Int], b: Expr[Int]) = BinOpE(AddB(), a, b)
  def infix_-(a: Expr[Int], b: Expr[Int]) = BinOpE(SubB(), a, b)
  def infix_*(a: Expr[Int], b: Expr[Int]) = BinOpE(MulB(), a, b)
  def infix_/(a: Expr[Int], b: Expr[Int]) = BinOpE(DivB(), a, b)

  implicit def intToExp(i: Int): Expr[Int] = NumE(i)
  implicit def stringToExp(s: String): Expr[String] = StringE(s)
}
