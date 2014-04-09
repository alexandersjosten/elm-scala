package elm

import scala.util.parsing.combinator._
import scala.collection.mutable.MutableList

object ElmParser extends RegexParsers {

  override val skipWhitespace = false

  val ident: Parser[String] = "[a-z][A-Za-z0-9_]*".r
  val moduleIdent: Parser[Name] = rep1sep("[A-Z][A-Za-z0-9.]*".r, ".") ^^ (_ mkString ".")
  val moduleName: Parser[Name]  =
    lexeme("module") ~> lexeme(moduleIdent) <~ lexeme("where") <~ "\n"
  val importStmt: Parser[Import] = {
    val stmt = lexeme(moduleIdent) ^^ (n => Import(n, List()))
    lexeme("import") ~> stmt <~ lexeme("\n")
  }
  val functionName: Parser[FunDef] = {
    val fun = lexeme(ident) ^^ (n => FunDef(n, SimpleT(UnitT())))

    fun <~ "[ ]*:[^\n]*\n".r
  }
  
  val comments: Parser[Unit] = {
    lazy val f: Parser[Unit] = { ("-}" | "(?s).".r ~> f) ~> success(Unit) }

    val mc: Parser[Unit] = "{-" ~> f ~> success(Unit)
    val sc: Parser[Unit] = "--[^\n]*".r ~> success(Unit)

    rep (sc | mc) >> {
      case Nil    => "[\n]*".r ~> success(Unit)
      case _ :: _ => "[\n]+".r ~> success(Unit)
    }
  }

  val elmModule: Parser[ElmModule] = {

    //comments ~> (repsep(importStmt, comments) ^^ (is => new ElmModule("apa", is, List())))
    comments ~> (repsep(functionName, comments) ^^ (fs => new ElmModule("apa", List(), fs)))
    //comments ~> moduleName ~> comments ~> (repsep(importName, comments) ^^ (is => new ElmModule("apa", is, List())))

    //comments ~> (moduleName ^^ (n => new ElmModule(n, List(), List())))
  }

  // lexeme, parse all trailing whitespaces and tabs
  def lexeme[T](p: Parser[T]): Parser[T] = p <~ "[ \t]*".r

  def parseElm(s: String): ParseResult[ElmModule] = parse(elmModule, s)
  type Name = String

  sealed case class Import(s: Name, fns: List[Name])
  sealed case class FunDef(s: Name, ty: Type)
  sealed case class ElmModule(name: Name, imports: List[Import],
                              functions: List[FunDef])
}
