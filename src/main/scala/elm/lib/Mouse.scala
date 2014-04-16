package elm

object Mouse {
  val position: Expression[Signal[(Int, Int)]] = BuiltInE(Variable("Mouse.position"))
  val x: Expression[Signal[Int]] = BuiltInE(Variable("Mouse.x"))
  val y: Expression[Signal[Int]] = BuiltInE(Variable("Mouse.y"))
}
