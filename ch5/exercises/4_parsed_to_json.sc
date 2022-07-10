import os.write
trait StrWriter[T] { def write(values: T): String }
object StrWriter {
  implicit object WritePrimitive extends StrWriter[Boolean] {
    def write(values: Boolean): String = values.toString
  }
  implicit object WriteInt extends StrWriter[Int] {
    def write(values: Int): String = values.toString
  }
  implicit object WriteString extends StrWriter[String] {
    def write(values: String): String = values
  }
  implicit object WriteDouble extends StrWriter[Double] {
    def write(values: Double): String = values.toString
  }
}

implicit def WriteSeq[T](implicit w: StrWriter[T]) = new StrWriter[Seq[T]] {
  def write(values: Seq[T]): String = s"[${values.map(w.write).mkString(",")}]"
}

implicit def WriteTuple[T, V](implicit w1: StrWriter[T], w2: StrWriter[V]) =
  new StrWriter[(T, V)] {
    def write(values: (T, V)): String =
      s"[${w1.write(values._1)},${w2.write(values._2)}]"
  }

def writeToString[T](values: T)(implicit w: StrWriter[T]): String = {
  w.write(values)
}

println(writeToString(true))
println(writeToString(Seq(true, true, false)))
println(writeToString(Seq(Seq(true, false), Seq(false))))
println(writeToString[(Int, Boolean)]((1, true)))
println(writeToString[(Seq[Int], Seq[Boolean])]((Seq(1), Seq(true))))
println(
  writeToString[Seq[(Seq[Int], Seq[Boolean])]](
    Seq(
      (Seq(1), Seq(true)),
      (Seq(2, 3), Seq(false, true)),
      (Seq(4, 5, 6), Seq(false, true, false))
    )
  )
)
println(
  writeToString(
    Seq(
      (Seq(1), Seq((true, 0.5))),
      (Seq(2, 3), Seq((false, 1.5), (true, 2.5)))
    )
  )
)
