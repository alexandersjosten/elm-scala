package elm

object Text {
  def plainText: Expr[String => Element] =
    BuiltInE(Var("Text.plainText"))
  def asText[A]: Expr[A => String] =
    BuiltInE(Var("Text.asText"))
}
