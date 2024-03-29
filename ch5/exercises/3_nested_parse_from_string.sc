// Modify the typeclass-based `parseFromString` to take a JSON-like format,
// where lists are demarcated by square brackets w/ comma-sep elements.
// This should allow it to parse and construct arbitrarily deep nested
// data structures *automatically* via typeclass inference

trait StrParser[T] { def parse(s: String): T }
object StrParser {
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

// Could also be written more succintly as...
// def parseFromString[T: StrParser](s: String) = {
// but this also requires us to access our `parse` method like...
// `implicitly[StrParser[T]].parse`
def parseFromString[T](s: String)(implicit parser: StrParser[T]) = {
  parser.parse(s)
}

// Different from the `implicit object`s above (which are singletons).
// This is because we would need a *different `new StrParser[T]` and thus
// need a different `StrParser[Seq[T]]
implicit def ParseSeq[T](implicit p: StrParser[T]) = new StrParser[Seq[T]] {
  def parse(s: String) =
    splitExpressions(s).map(p.parse)
}

// It doesn't really matter that we call this ParseTuple, all that matters is that
// when something looks for a `StrParser[(T, V)]` (eg: `StrParser[(Int, Boolean)]`)
// it will find the dynamically generated (?) StrParser from this, since it's marked as `implicit`,
// and thus discoverable within this scope.
implicit def ParseTuple[T, V](implicit p1: StrParser[T], p2: StrParser[V]) =
  new StrParser[(T, V)] {
    def parse(s: String) = {
      // Multiple assignment, JS equiv would be `const {left, right} = s.split(',')`
      val List(left, right) = splitExpressions(s)
      (p1.parse(left), p2.parse(right))
    }
  }

// left off: need to write function to split expressions (not as simple as just splitting on commas since we can have nested structures)
// eg: remove brackets, then keep track of open and closed brackets to eventually form indices of left and right
def splitExpressions(s: String): Seq[String] = {
  assert(s.head == '[')
  assert(s.last == ']')
  val indices =
    collection.mutable.ArrayDeque
      .empty[Int] // Using a queue here bc we can constantly resize and then concat w/ seqs later on
  var openBrackets = 0
  for (i <- Range(1, s.length - 1)) {
    s(i) match {
      case '[' => openBrackets += 1
      case ']' => openBrackets -= 1
      case ',' =>
        if (openBrackets == 0) indices += i
      case _ => // do nothing
    }
  }
  // concatenate iterables
  val allIndices =
    Seq(0) ++ indices ++ Seq(
      s.length - 1
    ) // For `[[1],[true,false]], this would be List(0, 4, 17)
  for (i <- Range(1, allIndices.length).toList) // why are we using toList here?
    yield s.substring(
      allIndices(i - 1) + 1,
      allIndices(i)
    )
  // the above is ultimately splitting `s` based on where we found top-level commas
}

// val strSeq = splitExpressions("[true,false,true]")
val something = splitExpressions("[[1],[true]]")
println(something)
println(parseFromString[(Seq[Int], Seq[Boolean])]("[[1],[true,false]]"))
println(
  parseFromString[Seq[(Seq[Int], Seq[Boolean])]](
    "[[[2],[true]],[[2,3],[false,true]],[[4,5,6],[false,true,false]]]"
  )
)

