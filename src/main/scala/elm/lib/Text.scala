package elm

object Text {
  def plainText: Expression[String => Element] =
    BuiltInE(Variable("Text.plainText"))
  def asText[A]: Expression[A => String] =
    BuiltInE(Variable("Text.asText"))
}
