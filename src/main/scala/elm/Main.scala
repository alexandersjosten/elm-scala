package elm

object Main {
  def main(args : Array[String]) {
    println("Initial")

    object Example extends Elm {
      var a = 1
      var b = 2
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
