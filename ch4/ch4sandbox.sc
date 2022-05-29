// CH4: Scala Collections

// BUILDERS let you make immutable unknown length collections
// (The immutable part comes when you call result() I guess? Since before then you're just using the builder but you don't have a concrete Array yet)
val b = Array.newBuilder[Int];
b += 1
b += 2
assert(b.result().size == 2)
assert(b.result()(0) == 1)
assert(b.result()(1) == 2)

// FACTORY METHODS provide easy ways to instantiate collections in a more declarative
// way than builders
val helloArr =
  Array.fill(5)("hello") // creates Array with 5 members that all say "hello"
assert(helloArr.size == 5)
helloArr.map(str => assert(str == "hello"))

// like fill, but you can also use indices when creating each element of the array
Array.tabulate(5)(n => s"hello $n")
// Array("hello 0", etc...)

// concat two arrays
val biggerArr = Array(1, 2, 3) ++ Array(4, 5, 6)
assert(biggerArr.size == 6)

// TRANSFORMS take a collection and create a new, modified one. The original collections is unchanged.
val arr = Array(1, 2, 3).map(i => i * 2)
assert(arr(1) == 4)
// other examples: filter, take, drop, slice, distinct

// QUERIES let you search for elements in a collection
val firstEvenNum = Array(1, 2, 3, 4, 5, 6).find(i => i % 2 == 0 && i > 2)
assert(firstEvenNum == Some(value = 4))
// can also use exists to check if element exists in array

// AGGREGATIONS
val mkStringExample = Array(1, 2, 3, 4, 5).mkString(",")
assert(mkStringExample == "1,2,3,4,5")
val startAndEndDelimiterEx = Array(1, 2, 3).mkString("[", ",", "]")
assert(startAndEndDelimiterEx == "[1,2,3]")
// foldLeft is like reduce (0 is initial value, function is reducer)
val folded = Array(1, 2, 3).foldLeft(0)((x, y) => x + y)
assert(folded == 6)
val foldedShortHandVer = Array(1, 2, 3).foldLeft(0)(_ + _)
assert(folded == 6)
// imperative version of foldLeft
{
  var total = 0
  for (i <- Array(1, 2, 3, 4)) total += i
  assert(total == 10)
}
// groupBy groups collections into a Map of smaller collections
val grouped = Array(1, 2, 3, 4).groupBy(_ % 2)
// grouped: Map[Int, Array[Int]] = Map(
//  0 -> Array(2, 4),
//  1 -> Array(1, 3)
//)

// COMBINING OPERATIONS
// eg: check if a sudoku grid is valid
// but first, some prerequisite info to break this down:
// the following are equivalent
val twoDimArr = Array(Array(1, 2), Array(3, 4))
Range(0, 2).map(i => twoDimArr(i))
Range(0, 2).map(twoDimArr(_))
// so basically `_` is just a placeholder for whatever the parameter of a function
// would have been if we explicitly specified it. knowing that, we can use that
// underscore to access members of our 2d array like so:
Range(0, 2).map(
  twoDimArr(0)(_)
) // iterate over all members of first array in a very confusing way! (1 and 2 in this case)
Range(0, 2).map(
  twoDimArr(_)(0)
) // iterate over each first element of our arrays (1 and 3 in this case)

def isValidSudokuGrid(grid: Array[Array[Int]]): Boolean = {
  !Range(0, 9).exists { i =>
    val row =
      Range(0, 9).map(grid(i)(_)) // get first 9 elements of ith array in grid
    val col =
      Range(0, 9).map(grid(_)(i)) // get ith element of first 9 arrays in grid
    // eg for i = 0
    // grid((0 % 3) * 3 + 0 % 3)((0 / 3) * 3 + 0 / 3)
    // = grid(0)(0) = 0th row, 0th col
    // grid((0 % 3) * 3 + 1 % 3)((0 / 3) * 3 + 1 / 3)
    // = grid(0 * 3 + 1 % 3)(0 * 3 + 1 / 3)
    // = grid(1)(0)
    // etc...
    val square =
      Range(0, 9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3))
    // board is valid if each row/col/square has 9 unique numbers (distinct removes non-unique members)
    row.distinct.length != row.length || col.distinct.length != col.length || square.distinct.length != square.length
  }
}

assert(
  isValidSudokuGrid(
    Array(
      Array(5, 3, 4, 6, 7, 8, 9, 1, 2),
      Array(6, 7, 2, 1, 9, 5, 3, 4, 8),
      Array(1, 9, 8, 3, 4, 2, 5, 6, 7),
      Array(8, 5, 9, 7, 6, 1, 4, 2, 3),
      Array(4, 2, 6, 8, 5, 3, 7, 9, 1),
      Array(7, 1, 3, 9, 2, 4, 8, 5, 6),
      Array(9, 6, 1, 5, 3, 7, 2, 8, 4),
      Array(2, 8, 7, 4, 1, 9, 6, 3, 5),
      Array(3, 4, 5, 2, 8, 6, 1, 7, 9)
    )
  ) == true
)

// CONVERTERS let you convert one collection type to another
// Array to Vector
val vec = Array(1, 2, 3).to(Vector)
val set = Array(1, 2, 3, 4).to(Set)

// VIEWS let you apply operations to a collection without traversing multiple times.
// Need to use `to` to convert back to a collection
val newArr =
  Array(1, 2, 3, 4, 5).view.map(_ + 1).filter(_ % 2 == 0).slice(0, 2).to(Array)
assert(newArr.sameElements(Array(2, 4)))

// VECTORS are fixed-size immutable linear sequences.
// Good general purpose sequence data structure which provides O(logn) perf for most operations
val v = Vector(1, 2, 3, 4)
assert(v(0) == 1)
val v2 = v.updated(2, 10)
assert(v2(2) == 10)
assert(v(2) == 3)

val v3 = Vector[Int]()
val v4 = v :+ 1 // new vector with 1 appended
val v5 = 4 +: v4 // new vector with 4 prepended
val v6 = v5.tail
assert(v6(0) == 1)

// LISTS are an immutable singly-linked list data structure which terminate in a Nil node
val myList = List(1, 2, 3, 4)
assert(myList.head == 1)
assert(myList.tail.head == 2)
assert(myList(0) == 1) // slower than other DS index lookups
