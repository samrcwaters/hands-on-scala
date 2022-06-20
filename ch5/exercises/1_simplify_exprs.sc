// Define a function that uses pattern matching on the Exprs we
// saw earlier to perform simple algebraic simplifications, eg:

// 1: evaluation
// (1 + 1) --> 2
// 2: evaluation + simplification
// ((1 + 1) * x) --> (2 * x)
// 3: identity?
// ((2 - 1) * x) --> x
// 4: recognizing that x*0 == 0, simplifying
// (((1 + 1) * y) + ((1 - 1) * x)) --> (2 * y)

{
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
