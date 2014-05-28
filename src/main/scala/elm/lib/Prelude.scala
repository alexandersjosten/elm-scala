package elm

object Prelude {
  def lift[A, B]: Expr[((A => B), Signal[A]) => Signal[B]] =
    BuiltInE(Var("Signal.lift"))

  def lift2[A, B, C]: Expr[(((A, B) => C), Signal[A], Signal[B]) => Signal[C]]
    = BuiltInE(Var("Signal.lift2"))

  def max[A]: Expr[(A, A) => A] = BuiltInE(Var("Basics.max"))
}
