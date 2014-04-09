package elm

import scala.util.parsing.combinator._

object ElmParser extends RegexParsers {

  override val skipWhitespace = false

  val ident: Parser[String] = "[a-z][A-Za-z0-9_]*".r
  val moduleIdent: Parser[Name] = rep1sep("[A-Z][A-Za-z0-9.]*".r, ".") ^^ (_ mkString ".")
  val moduleName: Parser[Name]  =
    lexeme("module") ~> lexeme(moduleIdent) <~ lexeme("where") <~ "\n"
  val importName: Parser[Name] = lexeme("import") ~> lexeme(moduleIdent) <~ "\n"
  val functionName: Parser[Name] = lexeme(ident) <~ "[ ]*:[^\n]*\n".r

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
    comments ~> (moduleName ^^ (n => new ElmModule(n, List(), List())))
  }

  // lexeme, parse all trailing whitespaces and tabs
  def lexeme[T](p: Parser[T]): Parser[T] = p <~ "[ \t]*".r

  def parseElm(s: String): ParseResult[ElmModule] = parse(elmModule, s)

  type Name = String

  sealed class Import(s: Name, open: Boolean)
  sealed class FunDef(s: Name, ty: Type)

  sealed class ElmModule(name: Name, imports: List[Import], 
                         functions: List[FunDef])

}
