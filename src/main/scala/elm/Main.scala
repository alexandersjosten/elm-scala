package elm

import Text._
import Prelude._
import java.io.File

object Main {

  def main(args: Array[String]) {
    object Example1 extends Elm {
      var a = (x: Expr[Int]) => x + 1
      var main = lift(a, Mouse.x)
    }

    object Example2 extends Elm {
      var edgeLength = lift2(max[Int], Mouse.x, Mouse.y)
      var resizeableYogi = (n: Expr[Int]) => image(n, n, "./yogi.jpg")
      var main = lift(resizeableYogi, edgeLength)
    }

    object Example3 extends Elm {
      import Color._

      var hand = (clr: Expr[Color], len: Expr[Int], time: Expr[Int]) => {
        val angle = degrees(90 - 6 * inSeconds(time))
        traced(solid(clr), segment((0,0), (len*cos(angle), len*sin(angle))))
      }

      var x: Expr[List[Int]] = List[Int](1, 2, 3)
      var f = (t: Expr[Int]) => collage(400, 400, List( filled(lightGrey, ngon(12, 110))
                                                      , outlined(solid(grey), ngon(12, 110))
                                                      , hand(orange, 100, t)
                                                      , hand(charcoal, 100, t/60)
                                                      , hand(charcoal, 60, t/720)))
      var y = lift(f, every(second))
    }

    val progStr = Example3.prog.emit

    printToFile(new File("target/elmtest.html"))(_.println(progStr))
    println(progStr)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

}
