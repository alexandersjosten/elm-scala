package elm

import Text._
import Prelude._
import java.io.File

object Main {

  def main(args : Array[String]) {
    object Example extends Elm {
      var a = (x: Expression) => x
      var b = (x: Expression) => x
      var main = lift(asText, a(b(Mouse.x)))
    }

    val progStr = Example.prog.emit

    printToFile(new File("target/elmtest.html"))(_.println(progStr))
    println(progStr)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
