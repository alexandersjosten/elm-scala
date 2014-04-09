package elm

import Text._
import Prelude._
import java.io.File
import ElmParser._

object Main {

  def main(args: Array[String]) {

    val file = io.Source.fromFile("Test.elm").mkString

    ElmParser.parseElm(file) match {
      case NoSuccess(msg, _)                => println(msg)
      case Success(ElmModule(n, is, fs), _) =>
        println("Module name: " + n + "\nImport statements: " + is + "\nFunctions: " + fs)
    }
  }
}
