for (i <- Range.inclusive(1, 100)) {
  if (i % 3 == 0 && i % 5 == 0) println("FizzBuzz")
  else if (i % 3 == 0) println("Fizz")
  else if (i % 5 == 0) println("Buzz")
  else println(i)
}

// alternate version
// for (i <- Range.inclusive(1, 100)) {
//   println(
//     if (i % 3 == 0 && i % 5 == 0) "FizzBuzz"
//     else if (i % 3 == 0) "Fizz"
//     else if (i % 5 == 0) "Buzz"
//     else i
//   )
// }