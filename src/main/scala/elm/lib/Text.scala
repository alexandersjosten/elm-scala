package elm

object Text {
  def plainText: Expr[String => Element] =
    BuiltInE(Variable("Text.plainText"))
  def asText[A]: Expr[A => String] =
    BuiltInE(Variable("Text.asText"))
}
