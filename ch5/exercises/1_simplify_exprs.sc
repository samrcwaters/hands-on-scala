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
    // evaluate expressions with only literals
    case BinOp(Literal(left), "+", Literal(right)) => Literal(left + right)
    case BinOp(Literal(left), "-", Literal(right)) => Literal(left - right)
    case BinOp(Literal(left), "*", Literal(right)) => Literal(left * right)
    // special cases for identity operation
    case BinOp(Literal(1), "*", right) => simplify(right)
    case BinOp(left, "*", Literal(1))  => simplify(left)
    case BinOp(left, "+", Literal(0))  => simplify(left)
    case BinOp(Literal(0), "+", right) => simplify(right)
    // special cases for zero property
    case BinOp(Variable(left), "*", Literal(0))  => Literal(0)
    case BinOp(Literal(0), "*", Variable(right)) => Literal(0)

    // Base cases for literal & something else
    case BinOp(Literal(left), op, Variable(right)) =>
      BinOp(Literal(left), op, Variable(right))
    case BinOp(Variable(left), op, Literal(right)) =>
      BinOp(Variable(left), op, Literal(right))
    // Other cases for literal & something else
    case BinOp(left, op, Variable(right)) =>
      simplify(BinOp(simplify(left), op, Variable(right)))
    case BinOp(Variable(left), op, right) =>
      simplify(BinOp(Variable(left), op, simplify(right)))
    case Variable(value) => Variable(value)
    case Literal(value)  => Literal(value)
    case BinOp(left, op, right) =>
      simplify(BinOp(simplify(left), op, simplify(right)))
  }

  // Our test cases
  // #1: (1 + 1) == 2
  val simp1 = stringify(simplify(BinOp(Literal(1), "+", Literal(1))))
  assert(simp1 == "2")
  println(simp1)
  println("hello?")
  // #2: ((1 + 1) * x) == (2 * x)
  val simp2 = stringify(
    simplify(BinOp(BinOp(Literal(1), "+", Literal(1)), "*", Variable("x")))
  )
  println(simp2)
  assert(
    simp2 == "(2 * x)"
  )
  // #3: (1 * x) == x
  val simp3 = stringify(
    simplify(
      BinOp(Literal(1), "*", Variable("x"))
    )
  )
  println(simp3)
  assert(simp3 == "x")
  // #3.5: (x * 1) == x
  val simp3point5 = stringify(
    simplify(
      BinOp(Literal(1), "*", Variable("x"))
    )
  )
  println(simp3point5)
  assert(simp3point5 == "x")
  // #4: ((2 - 1) * x) == x
  val simp4 = stringify(
    simplify(
      BinOp(BinOp(Literal(2), "-", Literal(1)), "*", Variable("x"))
    )
  )
  println(simp4)
  assert(simp4 == "x")
  // #5: zero property
  val zeroStrExpr = stringify(simplify(BinOp(Literal(0), "+", Literal(1))))
  println(zeroStrExpr)
  assert(zeroStrExpr == "1")
  // #6: two binops
  val twoBinOpsExpr = stringify(
    simplify(
      BinOp(
        BinOp(Literal(0), "+", Literal(1)),
        "-",
        BinOp(Literal(0), "+", Literal(1))
      )
    )
  )
  println(twoBinOpsExpr)
  // #6: ((( 1 + 1) * y) + (( 1 - 1) * x)) == (2 * y)
  val complicatedExpr = stringify(
    simplify(
      BinOp(
        BinOp(BinOp(Literal(1), "+", Literal(1)), "*", Variable("y")),
        "+",
        BinOp(BinOp(Literal(1), "-", Literal(1)), "*", Variable("x"))
      )
    )
  )
  println(complicatedExpr)
  assert(complicatedExpr == "(2 * y)")
}
