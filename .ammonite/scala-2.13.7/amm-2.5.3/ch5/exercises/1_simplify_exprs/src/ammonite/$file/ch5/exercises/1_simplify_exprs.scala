
package ammonite
package $file.ch5.exercises
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}


object `1_simplify_exprs`{
/*<script>*//*<amm>*/val res = /*</amm>*/{
  sealed trait Expr
  // binary operation could be addition, subtraction, multiplication, etc
  case class BinOp(left: Expr, op: String, right: Expr) extends Expr
  case class Literal(value: Int) extends Expr
  case class Variable(name: String) extends Expr

  def stringify(expr: Expr): String = expr match {
    case BinOp(left, op, right) =>
      s"(${stringify(left)} $op ${stringify(right)})"
    case Literal(value) => value.toString
    case Variable(name) => name
  }

  def evaluate(expr: Expr, values: Map[String, Int]): Int = expr match {
    case BinOp(left, "+", right) =>
      evaluate(left, values) + evaluate(right, values)
    case BinOp(left, "-", right) =>
      evaluate(left, values) - evaluate(right, values)
    case BinOp(left, "*", right) =>
      evaluate(left, values) * evaluate(right, values)
    case Literal(value) => value
    case Variable(name) => values(name)
  }

  def simplify(expr: Expr): Expr = expr match {
    case BinOp(Literal(left), "+", Literal(right)) => Literal(left + right)
    case BinOp(Literal(left), "-", Literal(right)) => Literal(left - right)
    case BinOp(Literal(left), "*", Literal(right)) => Literal(left * right)
    case BinOp(left, op, Variable(right)) =>
      BinOp(simplify(left), op, Variable(right))
  }

  // Our test cases
  // #1: evaluating an expression without variables
  assert(stringify(simplify(BinOp(Literal(1), "+", Literal(1)))) == "2")
  // #2: evaluation + a variable
  val simp2 = stringify(
    simplify(BinOp(BinOp(Literal(1), "+", Literal(1)), "*", Variable("x")))
  )
  println(simp2)
  assert(
    simp2 == "(2 * x)"
  )
}
/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "1_simplify_exprs"
  /*</generated>*/
}
