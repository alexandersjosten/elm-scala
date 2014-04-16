package elm

import scala.util.parsing.combinator._
import token._

object ElmParser extends RegexParsers {

  def EOF = "\\Z".r
  override val skipWhitespace = false

  val ident: Parser[String] = "[a-z][A-Za-z0-9_]*".r
  val moduleIdent: Parser[Name] = rep1sep("[A-Z][A-Za-z0-9.]*".r, ".") ^^ (_ mkString ".")
  val moduleName: Parser[Name]  =
    lexeme("module") ~> lexeme(moduleIdent) <~ lexeme("where") <~ "\n"
  val importStmt: Parser[Import] = {
    val stmt = lexeme(moduleIdent) ^^ (n => Import(n, List()))
    lexeme("import") ~> stmt <~ lexeme("\n")
  }
  val functionName: Parser[FunDef[Unit]] = {
    val fun = lexeme(ident) ^^ (n => FunDef(n, SimpleT(UnitT())))
    fun <~ "[ ]*:[^\n]*\n".r
  }

  val typeParser: Parser[ParserType] = {
    val singleVar = (lexeme(typeIdent) ^^ (t => Var(t)))
    val parenType = lexeme("(") ~> typeParser <~ lexeme(")")
    val fun = ((singleVar | parenType) <~ lexeme("->")) ~ typeParser ^^ {
      case ~(v, Fun(ts)) => Fun(v :: ts)
      case ~(v, ts)      => Fun(v, ts)
    }
    
    fun | parenType | singleVar
  }
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
    val commentModule = comments ~> moduleName
    val commentImport = comments ~> repsep(importStmt, comments)
    val commentFunction = comments ~> repsep(functionName, comments)


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
  sealed case class FunDef[+T](s: Name, ty: Type[T])
  sealed case class ElmModule(name: Name, imports: List[Import],
                              functions: List[FunDef[Any]])
}
