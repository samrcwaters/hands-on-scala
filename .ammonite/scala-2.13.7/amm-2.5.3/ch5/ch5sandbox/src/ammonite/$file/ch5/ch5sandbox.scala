
package ammonite
package $file.ch5
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


object ch5sandbox{
/*<script>*/import scala.util.Random
import scala.concurrent.ExecutionContext
import scala.collection.StringParsers

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
/*<amm>*/val res_5 = /*</amm>*/assert(p.x == 1)
/*<amm>*/val res_6 = /*</amm>*/assert(p.z == 3)
/*<amm>*/val res_7 = /*</amm>*/assert(p.toString == "PointCC(1,2)")
val p2 = p.copy(x = 10)
/*<amm>*/val res_9 = /*</amm>*/assert(p2.x == 10)

// SEALED TRAITS only allow a fixed set of classes to inherit from them
// - "open" = any number of classes can inherit from a trait
/*<amm>*/val res_10 = /*</amm>*/{
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
/*<amm>*/val res_11 = /*</amm>*/{
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
/*<amm>*/val res_14 = /*</amm>*/for (i <- Range.inclusive(1, 100)) {
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
/*<amm>*/val res_16 = /*</amm>*/assert(splitDate("01-01-2001") == "day: 01, mon: 01, yr: 2001")
/*<amm>*/val res_17 = /*</amm>*/assert(splitDate("hiiiii") == "not a valid date")

// pattern matching in for loop to destructure variables
val a = Array[(Int, String)]((1, "one"), (2, "two"))
// for ((i, s) <- a) println(s + i)

// similar unpacking with val statements
/*<amm>*/val res_19 = /*</amm>*/{
  val p3 = PointCC(123, 456)
  val PointCC(x, y) = p3
  assert(x == 123)
  assert(y == 456)
}

val s"$first $second" = "Hello World"
/*<amm>*/val res_21 = /*</amm>*/assert(first == "Hello")
/*<amm>*/val res_22 = /*</amm>*/assert(second == "World")

// Pattern matching on sealed traits and case classes.
// Example is a simple sealted trait that represents arithmetic expressions
/*<amm>*/val res_23 = /*</amm>*/{
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

// By-name parameters are useful for
// 1. Avoiding evaluation when not necessary
// 2. wrapping evaluation to run setup & teardown code before and after evaluation
// 3. repeating evaluation of the argument more than once

// Repeated Evaluation
// This function takes a by-name parameter f which produces a value of type T.
// Can wrap functions of any type in this.
def retry[T](max: Int)(f: => T): T = {
  var tries = 0
  var result: Option[T] = None
  while (result == None) {
    try {
      result = Some(f)
    } catch {
      // only run this partial function if the error we're catching is a Throwable!
      // note that you should never use curly braces in a case statment
      case e: Throwable =>
        tries += 1
        if (tries > max) throw e
        else {
          println(s"failed, retry #$tries")
        }
    }
  }
  result.get
}

val httpbin = "https://httpbin.org"
/*<amm>*/val res_26 = /*</amm>*/retry(max = 5) {
  // only succeeds w/ 200 response code 1/3 of the time
  requests.get(s"$httpbin/status/200,400,500")
}

// Implicit Parameters
class Foo(val value: Int)
def bar(implicit foo: Foo) = foo.value + 10
implicit val foo: Foo = new Foo(1)
// Implicit parameters are useful for times when you pass the same
// argument around a bunch of times (eg: execution context).
// This is better than globals for obvious reasons and better
// than thread local variables because you get compile-time error-checking

// 5.5: Typeclasses
// eg: parsing command line arguments all given as strings.
// Rather than other clunkier solutions, we can use the `implicit` keyword to essentially
// override some parent trait depending on the parameter type.
trait StrParser[T] { def parse(s: String): T }
object StrParser {
  // an object w/ the same name as a class that it is defined next to is called a companion object.
  // these are often used to group together implicits, statics, factory methods, and other functionality that is related to a trait or class
  // but does not belong to a specific instance.

  // This is essentially equivalent to saying "if anyone looks for a StrParser[Int]
  // they'll find this implicit object and use the instructions it provided to
  // convert something from a string into an int"
  implicit object ParseInt extends StrParser[Int] {
    def parse(s: String) = s.toInt
  }
  implicit object ParseBoolean extends StrParser[Boolean] {
    def parse(s: String) = s.toBoolean
  }
  implicit object ParseDouble extends StrParser[Double] {
    def parse(s: String) = s.toDouble
  }
}

// the above allows us to write a generic function that automatically uses the correct instance of StrParser depending on the type we asked it to parse
def parseFromString[T](s: String)(implicit parser: StrParser[T]) = {
  parser.parse(s)
}

val args = Seq("123", "true", "7.5")
// remember that the implicit keyword means we don't actually need to explicitly pass an argument called `parser` as long as it
// exists in the enclosing scope.
val myInt = parseFromString[Int](args(0))
val myBoolean = parseFromString[Boolean](args(1))
val myDouble = parseFromString[Double](args(2))

// re-using StrParser[T]s
def parseFromConsole[T](implicit parser: StrParser[T]) = {
  parser.parse(scala.Console.in.readLine())
}

// val myInt = parseFromConsole[Int]

// 5.5.3.2: Context-Bound Syntax
// the following are equivalent
// def parseFromString[T](s: String)(implicit parser: StrParser[T]) = ...
// def parseFromString[T: StrParser](s: String) = ...

// 5.5.4.1: Parsing Sequences
// One level of abstraction up from our StrParser[T] example!
// The following ParseSeq function provides a StrParser[Seq[T]] for any T which itself has an implicit StrParser[T] in scope.
implicit def ParseSeq[T](implicit p: StrParser[T]) = new StrParser[Seq[T]] {
  // Sequence-specific logic using pre-defined parser p
  def parse(s: String) = s.split(',').toSeq.map(p.parse)
}

val parsedSeq = parseFromString[Seq[Boolean]](
  "true,false,true"
) // ArraySeq(true, false, true)

// Can similarly parse tuples of type `key=value` by using one parser for the key and another for the value
// Can then use ParseSeq and ParseTuple to parse sequences of tuples or tuples of sequences
/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "ch5sandbox"
  /*</generated>*/
}
