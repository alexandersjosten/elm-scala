package elm

import scala.collection.mutable.MutableList
import scala.collection.mutable.StringBuilder

class Program {
  private var mapping: Map[Int, Expression] = Map[Int, Expression]()
  private var varId: Int = -1
  private var lamId: Int = -1

  def addExpr(expr: Expression): Expression = {
    val id = getVar
    mapping += id -> expr
    VarE(getName(id))
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

  def __newVar[T](expr: Expression): Expression = prog.addExpr(expr)
  def __newVar[T](fun: Expression => Expression): Expression = {
    val id   = prog.getLam
    val x    = VarE(id)
    val body = fun(x)
    val expr = LambdaE(id, SimpleT(UnitT()), body)

    prog.addExpr(expr)
  }

  implicit def intToExp(i: Int): Expression = NumE(i)
  implicit def stringToExp(s: String): Expression = StringE(s)
}
