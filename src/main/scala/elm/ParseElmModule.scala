package elm

import ElmParser._

object ParseElmModule {
  def parseModule(em: ElmModule): String = {
    var retStr: String = "package elm\n\n"
    var funStr = ""

    em.stmts.foreach( {
      case FunS(fd) => {
        funStr += "  def " + fd.s
        val variables = findVars(fd.ty)

        if(variables.isEmpty) {
          funStr += ": "
        }
        else {
          funStr += "["
          variables.foreach((v: String) => funStr += v + ",")
          funStr = funStr.dropRight(1) + "]: "
        }

        funStr += fd.ty.stringVal(fd.ty)
        funStr += " =\n    BuiltInE(Var(\"" + em.name + "." + fd.s + "\"))\n\n"
      }
      case DataS(n) => retStr += "class " + n + "\n"
    })

    retStr += "object " + em.name + " {\n"

    retStr += funStr + "}\n"
    retStr
  }

  def findVars(sig: ParserType): Set[String] = {
    var set: Set[String] = Set()
    sig match {
      case VarP(s)       =>
        if(Character.isLowerCase(s.charAt(0))) {
          set = set + (Character.toUpperCase(s.charAt(0)) + s.substring(1))
        }
      case TupleType(ts) => ts.foreach((pt: ParserType) => set = set ++ findVars(pt))
      case Fun(ts)       => ts.foreach((pt: ParserType) => set = set ++ findVars(pt))
      case App(l, r)     =>
        set = set ++ findVars(l)
        set = set ++ findVars(r)
    }

    set
  }
}
