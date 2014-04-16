package elm

object Prelude {
  def lift[A, B]: Expr[((A => B), Signal[A]) => Signal[B]] =
    BuiltInE(Var("Signal.lift"))
}
