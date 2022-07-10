// Merge two unsorted arrays, returning a sorted array.
def merge(left: Array[Int], right: Array[Int]) = {
  val (leftPtr, rightPtr) = (0, 0)
  // step through each element of each array and add the smaller element to our output array
  while (leftPtr < )
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
  }
}

// test cases
val sorted = mergeSort(Array(3, 2, 1))
println(sorted.mkString(","))
