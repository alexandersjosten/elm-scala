package elm

import scala.util.parsing.combinator._

object ElmParser extends RegexParsers {

  override val skipWhitespace = false

  val ident: Parser[String] = "[a-z][A-Za-z0-9_]*".r
  val moduleIdent: Parser[Name] = "[A-Z][A-Za-z0-9.]*".r
  val moduleName: Parser[Name]  =
    "module " ~> (rep1sep(moduleIdent, ".") ^^ (_ mkString ".")) <~ " where"

  val comments: Parser[Unit] = {
    lazy val f: Parser[Unit] = { ("-}" | "(?s).".r ~> f) ~> success(Unit) }

    val mc: Parser[Unit] = "{-" ~> f ~> success(Unit)
    val sc: Parser[Unit] = "--[^\n]*".r ~> success(Unit)

    rep (sc | mc) >> {
      case Nil    => success(Unit)
      case _ :: _ => "\n" ~> success(Unit)
    }
  }

  val elmModule: Parser[ElmModule] = {
    val name = comments ~> moduleName

    comments ~> (name ^^ (n => (n, List(), List())))
  }

  def parseElm(s: String) = parse(elmModule, s)

  type Name = String

  sealed class Import(s: Name, open: Boolean)
  sealed class FunDef(s: Name, ty: Type)

  type ElmModule = (Name, List[Import], List[FunDef])

}
