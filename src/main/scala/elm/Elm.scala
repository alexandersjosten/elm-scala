package elm

import scala.collection.mutable.MutableList
import scala.collection.mutable.StringBuilder

class Program {
  private var mapping : Map[Int, Expression] = Map[Int, Expression]()
  private var id : Int = -1

  def addExpr(expr : Expression) : Int = {
    id += 1
    mapping += id -> expr
    return id
  }

  def emit : String = {
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
    return str.toString
  }

  private def indent(depth : Int, s : String) : String = 
    " " * depth + s

  private def genProgram : List[Statement] = {
    val stmts = MutableList[Statement]()
    val program = mapping.init

    program.foreach {
      case (n, expr) =>
        val name = "x" + n
        stmts += InitS(name, expr)
    }

    stmts += InitS("main", mapping.last._2)

    return stmts.toList
  }
}

class Elm {
  val prog : Program = new Program()
  def __newVar[T](expr : Expression) : Int = prog.addExpr(expr)
  implicit def intToExp(i : Int) : Expression = NumE(i)
  implicit def stringToExp(s : String) : Expression = StringE(s)
}
