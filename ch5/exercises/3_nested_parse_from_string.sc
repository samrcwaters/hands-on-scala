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
  def parse(s: String) = s.split(',').toSeq.map(p.parse)
}

implicit def ParseTuple[T, V](implicit p1: StrParser[T], p2: StrParser[V]) =
  new StrParser[(T, V)] {
    def parse(s: String) = {
      // Multiple assignment, JS equiv would be `const {left, right} = s.split('=')`
      val Array(left, right) = s.split('=')
      (p1.parse(left), p2.parse(right))
    }
  }
