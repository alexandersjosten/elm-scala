package elm

object Prelude {
  def lift[A, B]: Expression[((A => B), Signal[A]) => Signal[B]] =
    BuiltInE(Variable("Signal.lift"))
}
