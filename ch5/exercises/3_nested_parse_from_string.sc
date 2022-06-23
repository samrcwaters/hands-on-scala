// Modify the typeclass-based `parseFroMString` to take a JSON-like format,
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
    s.substring(1, s.size - 1).split(',').toSeq.map(p.parse)
}

// It doesn't really matter that we call this ParseTuple, all that matters is that
// when something looks for a `StrParser[(T, V)]` (eg: `StrParser[(Int, Boolean)]`)
// it will find the dynamically generated (?) StrParser from this, since it's marked as `implicit`,
// and thus discoverable within this scope.
implicit def ParseTuple[T, V](implicit p1: StrParser[T], p2: StrParser[V]) =
  new StrParser[(T, V)] {
    def parse(s: String) = {
      // Multiple assignment, JS equiv would be `const {left, right} = s.split(',')`
      val Array(left, right) = s.substring(1, s.size - 1).split(',')
      (p1.parse(left), p2.parse(right))
    }
  }

// left off: need to write function to split expressions (not as simple as just splitting on commas since we can have nested structures)
// eg: remove brackets, then keep track of open and closed brackets to eventually form indices of left and right

// what's really important here is that we pick up things of the form "[something, something, etc...]"
// and pass off their members to another `parse` function that knows how to handle them, similar to what's
// done above in ParseTuple and ParseSeq

// sequence
println(
  parseFromString[Seq[Boolean]]("[true,false,true]")
) // should be List(true, false, true) (of type Seq[Boolean] (why? is this interchangeable with List?))
// tuple
println(parseFromString[(Int, Boolean)]("[2,true]")) // should be (2,true)
// tuple of seqs
println(parseFromString[(Seq[Int], Seq[Boolean])]("[[1],[true,false]]"))
// println(
//   parseFromString[Seq[(Seq[Int], Seq[Boolean])]](
//     "[[[2],[true]],[[2,3],[false,true]],[[4,5,6],[false,true,false]]]"
//   )
// )
