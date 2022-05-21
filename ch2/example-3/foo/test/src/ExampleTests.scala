package foo
import utest._
object ExampleTests extends TestSuite {
  def tests = Tests {
    test("hello") {
      val result = Example.hello()
      assert(result == "Hello World")
      result
    }
    test("goodbye") {
      val result = Example.goodbye()
      assert(result == "Goodbye!!")
      result
    }
  }
}
