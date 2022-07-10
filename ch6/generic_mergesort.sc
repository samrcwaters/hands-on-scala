// this generic signature is an example of context-bound syntax from ch5.
// which just means that items can be of any type as long as there exists
// an implicit Ordering[T] in scope.
def merge[T: Ordering](
    left: IndexedSeq[T],
    right: IndexedSeq[T]
): IndexedSeq[T] = {
  var (leftIdx, rightIdx) = (0, 0)
  val output = IndexedSeq.newBuilder[T]
  while (leftIdx < left.length || rightIdx < right.length) {
    val takeLeft = (leftIdx < left.length, rightIdx < right.length) match {
      case (true, false) => true
      case (false, true) => false
      case (true, true)  => Ordering[T].lt(left(leftIdx), right(rightIdx))
    }
    if (takeLeft) {
      output += left(leftIdx)
      leftIdx += 1
    } else {
      output += right(rightIdx)
      rightIdx += 1
    }
  }
  output.result()
}

// Remember that `T: Ordering` translates to "there should be an implicit `Ordering[T] in the current scope"
def mergeSort[T: Ordering](items: IndexedSeq[T]): IndexedSeq[T] = {
  if (items.length <= 1) items
  else {
    val (left, right) = items.splitAt(items.length / 2)
    val (sortedLeft, sortedRight) = (mergeSort(left), mergeSort(right))
    merge(sortedLeft, sortedRight)
  }
}

// @ mergeSort(Array(5, 4, 2, 10, 20))
// res0: IndexedSeq[Int] = Vector(2, 4, 5, 10, 20)
//
// @ mergeSort(Vector(5, 3, 4, 10, 20))
// res1: IndexedSeq[Int] = Vector(3, 4, 5, 10, 20)
//
// @ mergeSort(Vector("apple", "banana", "orange", "mango"))
// res2: IndexedSeq[String] = Vector("apple", "banana", "mango", "orange")
