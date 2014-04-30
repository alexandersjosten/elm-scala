package elm

import ElmParser._

object ParseElmModule {
  def parseModule(em: ElmModule): String = {
    var retStr: String = "package elm\n\n"
    retStr += "object " + em.name + " {\n"
    em.functions.foreach((fd: FunDef[ParserType]) =>
      retStr += "def " + fd.s + ": " + fd.ty.stringVal(fd.ty) + "\n"
    )
    retStr += "}\n"
    retStr
  }
}

