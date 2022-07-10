// Merge two unsorted arrays, returning a sorted array.
// My naive version.
// def merge(left: Array[Int], right: Array[Int]): Array[Int] = {
//   var sorted: Array[Int] = Array()
//   var (leftPtr, rightPtr) = (0, 0)
//   // step through each element of each array and add the smaller element to our output array
//   while (leftPtr < left.size && rightPtr < right.size) {
//     if (left(leftPtr) <= right(rightPtr)) {
//       sorted = sorted :+ left(leftPtr)
//       leftPtr += 1
//     } else {
//       sorted = sorted :+ right(rightPtr)
//       rightPtr += 1
//     }
//   }
//   if (leftPtr == left.size)
//     sorted = sorted ++ right.slice(rightPtr, right.size)
//   else sorted = sorted ++ left.slice(leftPtr, left.size)
//   sorted
// }

// book version
def merge(left: Array[Int], right: Array[Int]): Array[Int] = {
  var (leftIdx, rightIdx) = (0, 0)
  val output = Array.newBuilder[Int]
  while (leftIdx < left.length || rightIdx < right.length) {
    val takeLeft = (leftIdx < left.length, rightIdx < right.length) match {
      case (true, false) => true
      case (false, true) => false
      case (true, true)  => left(leftIdx) < right(rightIdx)
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

// Can only be O(n) levels of splitting for an array of size n.
// Each level takes O(n) time to split and merge back together:
//// This is the case because each merge step requires iterating through at most every item of the array,
//// so we round up and say that we need to consider n elements in the worst case
def mergeSort(items: Array[Int]): Array[Int] = {
  if (items.length <= 1) items
  else {
    val (left, right) = items.splitAt(items.length / 2)
    val (sortedLeft, sortedRight) = (mergeSort(left), mergeSort(right))
    merge(sortedLeft, sortedRight)
  }
}

// test cases
// merge
// val merged = merge(Array(1), Array(2, 3))
// println(merged.mkString(","))

// mergesort
val merged = mergeSort(Array(4, 7, 1))
println(merged.mkString(","))
// val sorted = mergeSort(Array(3, 2, 1))
// println(sorted.mkString(","))
