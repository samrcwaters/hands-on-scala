// Sequence
// - special case of iterable collections
// - always a defined order of elements
val seq: Seq[Int] = Seq(1, 2, 3, 4)

// List
// - subclass of iterable
// - immutable linked list
val someDays = List("Sunday", "Monday", "Tuesday")

// Tuple
// - fixed number of elements, each with its own type
// - immutable
// - especially handy for returning multiple values from a method
val ingredient =
  (
    "Sugar",
    25
  ) // inferred type is (String, Int), shorthand for Tuple2[String, Int]
// - can have up to 22 elements
