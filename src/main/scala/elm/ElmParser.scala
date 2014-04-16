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
  val typeParser: Parser[ParserType] = {
    val singleVar = (lexeme(typeIdent) ^^ (t => Var(t)))
    val parenType = lexeme("(") ~> typeParser <~ lexeme(")")
    val fun = ((singleVar | parenType) <~ lexeme("->")) ~ typeParser ^^ {
      case ~(v, Fun(ts)) => Fun(v :: ts)
      case ~(v, ts)      => Fun(v, ts)
    }
    
    fun | parenType | singleVar
  }
  val functionBody: Parser[Unit] = {
    val fun = rep1(lexeme(ident)) ~> lexeme("=")
    fun ~> rep("[^\n]*[\n]".r ~> guard("[ \t]".r)) ~> success(Unit)
  }
  val function: Parser[FunDef[ParserType]] =
    functionHeader <~ "\n" <~ functionBody
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
    val commentModule = comments ~> moduleName
    val commentImport = comments ~> repsep(importStmt, comments)
    val commentFunction = comments ~> repsep(function, comments)


    (commentModule ~ commentImport ~ commentFunction) <~ EOF ^^ {
      case ~(~(name, imports), funs) => ElmModule(name, imports, funs)
    }

    //commentImport ^^ (is => new ElmModule("apa", is, List()))
    //comments ~> (repsep(functionName, comments) ^^ (fs => new ElmModule("apa", List(), fs)))

    //comments ~> (moduleName ^^ (n => new ElmModule(n, List(), List())))


  }

  // lexeme, parse all trailing whitespaces and tabs
  def lexeme[T](p: Parser[T]): Parser[T] = p <~ "[ \t]*".r

  def parseElm(s: String): ParseResult[ElmModule] = parse(elmModule, s)
  type Name = String

  sealed case class Import(s: Name, fns: List[Name])
  sealed case class FunDef[+T](s: Name, ty: ParserType)
  sealed case class ElmModule(name: Name, imports: List[Import],
                              functions: List[FunDef[Any]])
}
