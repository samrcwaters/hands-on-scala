// Can `yield` within a for loop to collect a buffer of values
// which return when the loop finishes

val nums =
  for (i <- Range(1, 10))
    yield i

for (i <- Range(0, 9))
  assert(nums(i) == i + 1)
