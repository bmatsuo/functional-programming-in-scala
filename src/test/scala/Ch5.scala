import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.fp.ch5._

class Ch5 extends FunSuite with Matchers {
  // everything is using foldRight or unfold.
  test("?.?: foldRight") {
    Stream.empty.foldRight(3)((_, _) => 2) should be (3)
    Stream(1, 2, 3).foldRight[Int](0)(_ + _) should be (6)
  }

  test("?.?: toList") {
    import Stream._
    Empty.toList should be (Nil)
    cons(1, cons(2, Empty)).toList should be (List(1, 2))
    Stream(1, 2, 3).toList should be (List(1, 2, 3))
  }

  test("?.?: unfold") {
    Stream.unfold(1)(_ => None).toList should be (Nil)
    Stream.unfold(true)(if (_) Some(1, false) else None).toList should be (List(1))
    Stream.unfold(1)(n => if (n < 1) None else Some((n, n - 1))).toList should be (List(1))
    Stream.unfold(2)(n => if (n < 1) None else Some((n, n - 1))).toList should be (List(2, 1))

    // unfold is lazy
    Stream.unfold(1)(_ => Some(0, 0))
    1 should be (1)
  }
}
