package elm

import Text._
import java.io.File

object Main {
  def main(args : Array[String]) {
    println("Initial")

    object Example extends Elm {
      var a = plainText("Hej")
    }

    val progStr = Example.prog.emit

    printToFile(new File("elmtest.html"))(p =>
      p.println(progStr)
    )
    println(progStr)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
