package elm

object Mouse {
  val position: Expr[Signal[(Int, Int)]] = BuiltInE(Variable("Mouse.position"))
  val x: Expr[Signal[Int]] = BuiltInE(Variable("Mouse.x"))
  val y: Expr[Signal[Int]] = BuiltInE(Variable("Mouse.y"))
}
