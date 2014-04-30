package elm

import ElmParser._

object ParseElmModule {
  def parseModule(em: ElmModule): String = {
    var retStr: String = "package elm\n\n"
    retStr += "object " + em.name + " {\n"
    em.functions.foreach({(fd: FunDef[ParserType]) =>
      retStr += "def " + fd.s
      val variables = findVars(fd.ty)

      if(variables.isEmpty) {
        retStr += ": "
      }
      else {
        retStr += "["
        variables.foreach((v: String) => retStr += v + ",")
        retStr = retStr.dropRight(1) + "]: "
      }

      retStr += fd.ty.stringVal(fd.ty) + "\n"
    })
    retStr += "}\n"
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

