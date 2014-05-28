package elm

class Time
class Path
class LineStyle
class Form
class Shape

object Prelude {
  def lift[A, B]: Expr[((A => B), Signal[A]) => Signal[B]] =
    BuiltInE(Var("Signal.lift"))

  def lift2[A, B, C]: Expr[(((A, B) => C), Signal[A], Signal[B]) => Signal[C]]
    = BuiltInE(Var("Signal.lift2"))

  def max[A]: Expr[(A, A) => A] = BuiltInE(Var("Basics.max"))

  def image: Expr[(Int, Int, String) => Element] = BuiltInE(Var("Graphics.Element.image"))

  def collage: Expr[(Int, Int, List[Form]) => Element]
    = BuiltInE(Var("Graphics.Collage.collage"))

  def filled: Expr[(Color, Shape) => Form] = BuiltInE(Var("Graphics.Collage.filled"))

  def ngon: Expr[(Int, Int) => Shape] = BuiltInE(Var("Graphics.Collage.ngon"))

  def segment: Expr[((Int,Int), (Int,Int)) => Path]
    = BuiltInE(Var("Graphics.Collage.segment"))

  def traced: Expr[(LineStyle, Path) => Form]
    = BuiltInE(Var("Graphics.Collage.traced"))

  def solid: Expr[Color => LineStyle]
    = BuiltInE(Var("Graphics.Collage.solid"))

  def outlined: Expr[(LineStyle, Shape) => Form]
    = BuiltInE(Var("Graphics.Collage.outlined"))

  def cos: Expr[Int => Int]
    = BuiltInE(Var("Basics.cos"))

  def sin: Expr[Int => Int]
    = BuiltInE(Var("Basics.sin"))

  def degrees: Expr[Int => Int]
    = BuiltInE(Var("Basics.degrees"))

  def inSeconds: Expr[Int => Int]
    = BuiltInE(Var("Time.inSeconds"))

  def second: Expr[Int]
    = BuiltInE(Var("Time.second"))

  def every: Expr[Int => Signal[Int]]
    = BuiltInE(Var("Time.every"))

}
