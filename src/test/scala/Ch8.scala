import com.fp.ch7.Par
import com.fp.ch7.Par.Par
import java.util.concurrent.{Executors, ExecutorService}
import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.fp.ch7._
import com.fp.ch8._

import scalaz._
import Scalaz._

class Ch8 extends FunSuite with Matchers {
  test("8.3: BoolProp") {
    def pass = new BoolProp { def check = true }
    def fail = new BoolProp { def check = false }
    (pass && pass).check should be (true)
    (pass && fail).check should be (false)
    (fail && pass).check should be (false)
    (fail && fail).check should be (false)
  }

  def smallInts = Gen.choose(-10, 10)

  test("?.? prop") {
    import Prop.EnrichedString

    Prop.run("unit is constant" := Prop.forAll(Gen.unit(1))(1 ==))
  }

  test("?.? max") {
    import SProp.EnrichedString

    SProp.run("no elements greater" := SProp.forAll(SGen.listOf1(smallInts)) { xs =>
      val max = xs.max
      !xs.exists(max <)
    }) should be (None)
  }

  test("?.? sorted") {
    import SProp.EnrichedString

    SProp.run("independence of initial order" := SProp.forAll(SGen.listOf1(smallInts)) { xs =>
      xs.sorted == xs.reverse.sorted
    }) should be (None)
  }

  test("?.? par unit") {

    SProp.run(SProp.check {
      val es: ExecutorService = Executors.newCachedThreadPool

      def equal[A](a1: Par[A], a2: Par[A]): Par[Boolean] =
        Par.map2(a1, a2)(_ == _)

      val p = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)

      equal(p, p2) (es) get
    }) should be (None)
  }
}
