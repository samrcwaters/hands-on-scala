// 3.2.3 FizzBuzz
def fizzBuzz = {
  for (i <- Range.inclusive(1, 100)) {
    if (i % 3 == 0 && i % 5 == 0) println("FizzBuzz")
    else if (i % 3 == 0) println("Fizz")
    else if (i % 3 == 0) println("Buzz")
    else println(i)
  }
}

// fizzBuzz // prints fizzBuzz output

// 3.3.2 Function Values
// functions ARE values, but methods are not. In that you can only pass around functions, store them in variables, etc.
/// 3.3.2.1 Methods taking functions
class Box(var x: Int) {
  def update(f: Int => Int) = x = f(x)
  def printMsg(msg: String) = {
    println(msg + x)
  }
}

// val b = new Box(1)
// b.printMsg("hello") // hello1
// b.update(i => i + 5)
// b.printMsg("hello") // hello6

/// 3.3.2.2 Multiple Parameter Lists
// - useful for higher-order methods that can be used like control structures
//  - basic control structures are conditionals (selection) and iteration (loops)
// I think we can think of this like so:
// myLoop is a function which takes start and end args, and returns
// a function which takes a callback arg, which we then apply to the
// range specified by our start and end.
// JS equivalent might be:

// function myLoop(start, end) {
//   return (callback) => {
//     for (let i=start; i<end; i++) {
//       callback(i)
//     }
//   }
// }

def myLoop(start: Int, end: Int)(callback: Int => Unit) = {
  for (i <- Range(start, end)) {
    callback(i)
  }
}

// myLoop(0, 5) { i =>
//   println(s"i: ${i}")
// }

// 3.4: Classes and Traits

class Foo(x: Int) {
  def printMsg(msg: String) = {
    println(msg + x)
  }
}

// val f = new Foo(1)
// f.printMsg("hello") // hello1

// x is a mutable class member because we declared it with var
class Qux(var x: Int) {
  def printMsg(msg: String) = {
    x += 1
    println(msg + x)
  }
}

// val q = new Qux(1)
// q.printMsg("hello") // hello2
// q.printMsg("hello") // hedy llo3

// using vals or vars in the body of a class.
// these get computed once at the time of class instantiation (when an instance of a class is created)
class Baz(x: Int) {
  val bangs = "!" * x
  def printMsg(msg: String) = {
    println(msg + bangs)
  }
}

// val z = new Baz(3)
// z.printMsg("hello") // hello!!!

// 3.4.1 Traits
// Similar to interfaces
trait Point { def hypotenuse: Double }

class Point2D(x: Double, y: Double) extends Point {
  def hypotenuse = math.sqrt(x * x + y * y)
}

class Point3D(x: Double, y: Double, z: Double) extends Point {
  def hypotenuse = math.sqrt(x * x + y * y + z * z)
}

// val points: Array[Point] = Array(new Point2D(1, 2), new Point3D(4, 5, 6))
// println(points(0).hypotenuse) // 2.23606797749979
// for (p <- points) println(p.hypotenuse)

// Excercise: flexible fizzbuzz.
// allow caller to either:
// - ignore the output
// - println the output
// - put the output in a pre-allocated array
def flexibleFizzBuzz(callback: String => Unit) = {
  for (i <- Range.inclusive(1, 100)) {
    if (i % 3 == 0 && i % 5 == 0) callback("FizzBuzz")
    else if (i % 3 == 0) callback("Fizz")
    else if (i % 3 == 0) callback("Buzz")
    else callback(i.toString)
  }
}

// empty callback function body
// flexibleFizzBuzz(s => {}) // does nothing with output
// flexibleFizzBuzz(s => println(s)) // prints the output
// puts the output in an array
// var i = 0
// val output = new Array[String](100)
// flexibleFizzBuzz(s => {
//   output(i) = s
//   i += 1
// })
// for (i <- output) println(i)

// Exercise 2: recursive method printMessages.
// takes an array of Msg class instances each with an optional parent ID
// print out a threaded fashion (child messages printed indented underneath parents, arbitrarily deep)
// Eg:
// #0 Hello
//   #1 World
class Msg(val id: Int, val parent: Option[Int], val txt: String)

// Unit is kind of like void
// what's our recursive step?
def printMessages(msgs: Array[Msg]): Unit = {
  def helper(index: Int): Unit = {
    val msg = msgs(index)
    println(s"#${msg.id} ${msg.txt}")
    // base case: at the end of array
    if (index < msgs.size - 1) helper(index + 1)
  }
  helper(0)
}

// def printMessagesHelper()

printMessages(
  Array(
    new Msg(0, None, "Hello"),
    new Msg(1, Some(0), "World"),
    new Msg(2, None, "I am Cow"),
    new Msg(3, Some(2), "Hear me moo"),
    new Msg(4, Some(2), "Here I stand"),
    new Msg(5, Some(2), "I am Cow"),
    new Msg(6, Some(5), "Here me moo, moo")
  )
)
