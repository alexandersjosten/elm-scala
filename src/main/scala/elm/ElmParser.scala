package elm

import scala.util.parsing.combinator._
import token._

object ElmParser extends RegexParsers {

  def EOF = "\\Z".r
  override val skipWhitespace = false

  val ident: Parser[String] = "[a-z][A-Za-z0-9_]*".r
  val typeIdent: Parser[String] = "[A-Za-z0-9_]+".r
  val moduleIdent: Parser[Name] = rep1sep("[A-Z][A-Za-z0-9.]*".r, ".") ^^ (_ mkString ".")
  val moduleName: Parser[Name]  =
    lexeme("module") ~> lexeme(moduleIdent) <~ lexeme("where") <~ "\n"
  val importStmt: Parser[Import] = {
    val stmt = lexeme(moduleIdent) ^^ (n => Import(n, List()))
    lexeme("import") ~> stmt <~ lexeme("\n")
  }

  /* functionHeader will parse the name of the function and create the type of
   * the function.
   * typeParser will parse the types.
   * functionBody will parse the body of the function, i.e. just throw away
   * everything
   * function will parse the given function and return a FunDef containing
   * the name and it's arguments and return type
   */
  val functionHeader: Parser[FunDef[ParserType]] =
    lexeme(ident) >> (n => lexeme(":") ~> typeParser ^^ (f => FunDef(n, f)))

  /* parser for the different types, can be of any of the following forms:
   * a
   * (a) -> b ..
   * a -> b ..
   * (a -> b) -> c
   */
  val typeParser: Parser[ParserType] = {
    val singleVar = (lexeme(typeIdent) ^^ (t => VarP(t)))
    val parenType = lexeme("(") ~> (rep1sep(typeParser, lexeme(",")) ^^ (ts =>
      ts.length match {
        case 1 => ts.head
        case _ => TupleType(ts)
      }
    )) <~ lexeme(")")

    val fun = ((singleVar | parenType) <~ lexeme("->")) ~ typeParser ^^ {
      case ~(v, Fun(ts)) => Fun(v :: ts)
      case ~(v, ts)      => Fun(v, ts)
    }
    
    fun | parenType | singleVar
  }
  val functionBody: Parser[Unit] = {
    val firstLine = rep1(lexeme(ident)) ~> lexeme("=") ~> "[^\n]*[\n]".r
    firstLine ~> rep(guard("[ \t]".r) ~> "[^\n]*[\n]".r) ~> success(Unit)
  }
  val function: Parser[FunDef[ParserType]] =
    functionHeader <~ "\n" <~ functionBody

  // parses the comments, both multiline and single line comments
  val comments: Parser[Unit] = {
    lazy val f: Parser[Unit] = { ("-}" | "(?s).".r ~> f) ~> success(Unit) }

    val mc: Parser[Unit] = "{-" ~> f ~> success(Unit)
    val sc: Parser[Unit] = "--[^\n]*".r ~> success(Unit)

    rep (sc | mc) >> {
      case Nil    => "[\n]*".r ~> success(Unit)
      case _ :: _ => "[\n]+".r ~> success(Unit)
    }
  }

  // parser the module, uses functions above
  val elmModule: Parser[ElmModule] = {
    val commentModule = comments ~> moduleName
    val commentImport = comments ~> repsep(importStmt, comments)
    val commentFunction = comments ~> repsep(function, comments)
    
    (commentModule ~ commentImport ~ commentFunction) ^^ {
      case ~(~(name, imports), funs) => ElmModule(name, imports, funs)
    }
  }

  // lexeme, parse all trailing whitespaces and tabs
  def lexeme[A](p: Parser[A]): Parser[A] = p <~ "[ \t]*".r

  def parseElm(s: String): ParseResult[ElmModule] = parse(elmModule, s)

  type Name = String

  sealed case class Import(s: Name, fns: List[Name])
  sealed case class FunDef[+T](s: Name, ty: ParserType)
  sealed case class ElmModule(name: Name, imports: List[Import],
                              functions: List[FunDef[ParserType]])
}
