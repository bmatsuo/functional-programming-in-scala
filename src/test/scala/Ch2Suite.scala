import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.fp.ch2._

class Ch2 extends FunSuite with Matchers {
  test("2.1: fib") {
    fib(0) should be (0)
    fib(1) should be (1)
    fib(2) should be (1)
    fib(3) should be (2)
    fib(4) should be (3)
    fib(5) should be (5)
  }

  test("2.2: isSorted") {
    isSorted[Int](Array(1, 2, 3, 4), _ > _) should be (true)
    isSorted[Int](Array(1, 3, 2, 4), _ > _) should be (false)
    isSortedList[Int](List(1, 2, 3, 4), _ > _) should be (true)
    isSortedList[Int](List(1, 3, 2, 4), _ > _) should be (false)
  }

  test("2.3: curry") {
    val f = curry[Int, Int, Int]((a, b) => a + b)
    f(1)(2) should be (3)
  }

  test("2.4: uncurry") {
    val f = uncurry[Int, Int, Int](curry((a, b) => a + b))
    f(1, 2) should be (3)
  }

  test("2.5: compose") {
    import scala.collection.immutable.StringOps

    val f = compose[Int, String, Int](
      _.toInt,
      compose[Int, String, String](
        new StringOps(_).reverse,
        i => s"$i"))

    f(123) should be (321)
  }
}
