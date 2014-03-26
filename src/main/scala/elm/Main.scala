package elm

object Main {
  def main(args : Array[String]) {
    println("Initial")

    object Example extends Elm {
      var a = 1
      var b = 2
    }

    println(Example.prog.emit)
  }
}
