/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package elm

import scala.language.implicitConversions

/**
  * Abstract syntax of JavaScript
  */
sealed abstract class JsStmt {
  def &(other: JsStmt) = JsStmtConcat(List(this, other))
}

object JsStmt {
  implicit def stmtsToStmt(l: List[JsStmt]): JsStmtConcat = JsStmtConcat(l)
}

case class JsStmtConcat(stmts: List[JsStmt]) extends JsStmt {
  override def &(other: JsStmt) = other match {
    case JsStmtConcat(l) => JsStmtConcat(stmts ++ l)
    case _ => JsStmtConcat(stmts :+ other)
  }
}

case class JsFunction(name: JsName, params: List[JsName], var body: JsStmt) extends JsStmt
case class JsDeclaration(name: JsName) extends JsStmt
case class JsDef(name: JsName, rhs: JsExpr) extends JsStmt
object Noop extends JsStmt
case class JsIf(conditiond: JsExpr, body: JsStmt) extends JsStmt
case class JsIfElse(conditiond: JsExpr, body: JsStmt, elseBody: JsStmt) extends JsStmt
case class JsWhile(conditiond: JsExpr, body: JsStmt) extends JsStmt
case class JsDoWhile(conditiond: JsExpr, body: JsStmt) extends JsStmt
case class JsFor(initial: JsStmt, condition: JsStmt, increment: JsStmt, body: JsStmt) extends JsStmt
//TODO check if JsStmt is in JsForIn really necessary
case class JsForIn(x: JsStmt, in: JsStmt, body: JsStmt) extends JsStmt
object JsBreak extends JsStmt
object JsContinue extends JsStmt
case class JsReturn(e: Option[JsExpr]) extends JsStmt {
  def this() = this(None)
  def this(e: JsExpr) = this(Some(e))
}
case class JsBlock(body: JsStmt) extends JsStmt
case class JsTryCatch(tryBody: JsStmt, catchVar: JsName, catchBody: JsStmt) extends JsStmt
case class JsThrow(exception: String) extends JsStmt
case class JsExprS(e: JsExpr) extends JsStmt

sealed abstract class JsExpr

object JsExpr {
  implicit def nameToJsExpr(s: String): JsName = JsName(s)
  implicit def numToJsExpr(n: Number): JsNum = JsNum(n)
  implicit def boolToJsExpr(b: Boolean): JsBool = JsBool(b)

  implicit def exprToStmt(e: JsExpr) = JsExprS(e)
}

case class JsArray(elems: List[JsExpr]) extends JsExpr
case class JsAssignment(lhs: JsExpr, rhs: JsExpr) extends JsExpr
case class JsName(name: String) extends JsExpr
case class JsQualifiedName(qualifier: JsName, name: JsName) extends JsExpr
case class JsNum(n: Number) extends JsExpr
case class JsStr(s: String) extends JsExpr
case class JsBool(b: Boolean) extends JsExpr
case class JsRaw(s: String) extends JsExpr
object JsNull extends JsExpr
case class JsNew(name: JsName, args: List[JsExpr]) extends JsExpr
case class JsFunctionCall(f: JsExpr, args: JsExpr*) extends JsExpr
case class JsMemberAccess(obj: JsExpr, arg: JsExpr) extends JsExpr
case class JsObject(fields: List[(JsName, JsExpr)]) extends JsExpr
case class JsAnonymousFunction(params: List[JsName], var body: JsStmt) extends JsExpr
case class JsBinOp(lhs: JsExpr, op: JsName, rhs: JsExpr) extends JsExpr
case class JsUnOp(op: JsName, e: JsExpr) extends JsExpr
case class JsUnit() extends JsExpr

object JsObject {
  def apply() = new JsObject(List())
}

object JsPrettyPrinter {
  def parens(s: String) = "(" + s + ")"

  def braces(s: String) = "{" + s + "}"

  def indent(s: String): String = {
    import scala.util.matching.Regex
    "  " + ("\n(.)".r replaceAllIn(s, "\n  $1"))
  }

  def prettyBlock(s: JsStmt) = s match {
    case JsStmtConcat(Nil) => "{}"
    case _ => "{" + indent("\n" + prettyStmt(s)) + "\n}"
  }

  def prettyStmt(js: JsStmt): String = js match {
    case JsStmtConcat(ss) => ss.map(prettyStmt).mkString("\n")
    case JsBlock(m) => prettyBlock(m)
    case JsExprS(e) => prettyExpr(e) + ";"
    case JsReturn(None) => "return;"
    case JsReturn(Some(e)) => "return " + prettyExpr(e) + ";"
    case JsIf(cond, body) => "if " + parens(prettyExpr(cond)) + " " + prettyBlock(body)
    case JsDef(n, e) => "var " + prettyExpr(n) + " = " + prettyExpr(e) + ";"
    case Noop => ""
    case _ => throw new RuntimeException("Not matched: " + js.toString)
  }

  def prettyExpr(js: JsExpr): String = js match {
    case JsArray(ls) => "[" + ls.map(prettyExpr).mkString(",") + "]"
    case JsAssignment(el, er) => prettyExpr(el) + " = " + prettyExpr(er)
    case JsName(s) => s
    case JsQualifiedName(q: JsName, name: JsName) => prettyExpr(q) + "." + prettyExpr(name)
    case JsNum(n) => n.toString
    case JsStr(s) => "\"" + s + "\""
    case JsBool(b) => b.toString
    case JsRaw(s) => s
    case JsFunctionCall(f, args @ _*) =>
      prettyExpr(f) + parens(args.map(prettyExpr).mkString(", "))
    case JsObject(m) => {
      def f(x: (JsName, JsExpr)) = JsBinOp(x._1, ":", x._2)

      braces(m.map(f).map(prettyExpr).mkString(", "))
    }
    case JsAnonymousFunction(args, body) =>
      "function " + parens(args.map(prettyExpr).mkString(", ")) + " " + prettyBlock(body)
    case JsBinOp(el, op, er) => prettyExpr(el) + " " + prettyExpr(op) + " " + prettyExpr(er)
    case JsUnit() => "()"

    case _ => throw new RuntimeException("Not matched: " + js.toString)
  }

}
