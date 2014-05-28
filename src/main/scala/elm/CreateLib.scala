package elm

import java.io.File
import ElmParser._
import ParseElmModule._

object CreateLib {
  /* Should have two args:
   *   first one is file to read from (.elm)
   *   second file to write to (.scala)
   */
  def main(args: Array[String]) {
    if(args.length != 2) {
      throw new IllegalArgumentException(
        "Wrong number of args, is " + args.length + ", should be 2"
      )
    }

    val inputName: String = args(0)
    val outputName: String = args(1)

    val file = io.Source.fromFile(inputName).mkString
    ElmParser.parseElm(file) match {
      case NoSuccess(msg, reader) => println("Parse error on: " + reader.pos + "\n" + msg)
      case Success(s,_)      =>
        printToFile(new File(outputName))(_.println(parseModule(s)))
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
