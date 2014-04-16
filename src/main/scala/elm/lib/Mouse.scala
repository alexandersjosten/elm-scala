package elm

object Mouse {
  val position: Expr[Signal[(Int, Int)]] = BuiltInE(Var("Mouse.position"))
  val x: Expr[Signal[Int]] = BuiltInE(Var("Mouse.x"))
  val y: Expr[Signal[Int]] = BuiltInE(Var("Mouse.y"))
}
