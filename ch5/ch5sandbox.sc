import scala.util.Random

// CASE CLASSES are kinda like C structs or Python data classes.
// - meant to just hold immutable data
// - can be instantiated w/o `new`
// - good replacements for large tuples so you don't need to use `._1` syntax to access members

// "CC" is just to avoid naming conflict with Point sealed trait below
case class PointCC(x: Int, y: Int) {
  def z = x + y
}
val p = PointCC(1, 2)
// fields public by default
assert(p.x == 1)
assert(p.z == 3)
assert(p.toString == "PointCC(1,2)")
val p2 = p.copy(x = 10)
assert(p2.x == 10)

// SEALED TRAITS only allow a fixed set of classes to inherit from them
// - "open" = any number of classes can inherit from a trait
{
  sealed trait Point
  case class Point2D(x: Double, y: Double) extends Point
  case class Point3D(x: Double, y: Double, z: Double) extends Point

  // define how each kind of point should be handled since we know exactly
  // what kinds to expect
  def hypotenuse(p: Point) = p match {
    case Point2D(x, y)    => math.sqrt(x * x + y * y)
    case Point3D(x, y, z) => math.sqrt(x * x + y * y + z * z)
  }
}

// json example
// hasn't changed in 20 years, so it's a pretty good use case for sealed traits :)
{
  sealed trait Json
  case class Null() extends Json
  case class Bool(value: Boolean) extends Json
  case class Str(value: String) extends Json
  case class Num(value: Double) extends Json
  case class Arr(value: Seq[Json]) extends Json
  case class Dict(value: Map[String, Json]) extends Json
}

// PATTERN MATCHING is the replacement for if/else statements
val x = Random.nextInt(10)

val y = x match {
  case 0 => "zero"
  case 1 => "one"
  case 2 => "two"
  case _ => "other"
}
// println(y)

// fizzbuzz using pattern matching
for (i <- Range.inclusive(1, 100)) {
  // match on tuple
  val s = (i % 3, i % 5) match {
    case (0, 0) => "FizzBuzz"
    case (0, _) => "Fizz"
    case (_, 0) => "Buzz"
    case _      => i
  }
  // println(s)
}

// validating a date string format
def splitDate(s: String) = s match {
  case s"$day-$month-$year" => s"day: $day, mon: $month, yr: $year"
  case _                    => "not a valid date"
}
assert(splitDate("01-01-2001") == "day: 01, mon: 01, yr: 2001")
assert(splitDate("hiiiii") == "not a valid date")

// pattern matching in for loop to destructure variables
val a = Array[(Int, String)]((1, "one"), (2, "two"))
// for ((i, s) <- a) println(s + i)

// similar unpacking with val statements
{
  val p3 = PointCC(123, 456)
  val PointCC(x, y) = p3
  assert(x == 123)
  assert(y == 456)
}

val s"$first $second" = "Hello World"
assert(first == "Hello")
assert(second == "World")

// Pattern matching on sealed traits and case classes.
// Example is a simple sealted trait that represents arithmetic expressions
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

  // stringifying
  val smallExpr = BinOp(Variable("x"), "+", Literal(1))
  assert(stringify(smallExpr) == "(x + 1)")

  val largerExpr = BinOp(
    BinOp(Variable("x"), "+", Literal(1)),
    "*",
    BinOp(Variable("y"), "-", Literal(1))
  )
  assert(stringify(largerExpr) == "((x + 1) * (y - 1))")

  // evaluating
  assert(evaluate(smallExpr, Map("x" -> 10)) == 11)
  assert(evaluate(largerExpr, Map("x" -> 10, "y" -> 20)) == 209)
}

// By-name parameters
