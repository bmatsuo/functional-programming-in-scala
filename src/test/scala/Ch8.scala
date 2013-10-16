import org.scalatest.FunSuite
import org.scalatest.Matchers
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

  test("?.? max") {
    val smallInts = Gen.choose(-10, 10)
    val smallNonNegative = Gen.choose(0, 10)

    Prop.forAll(Gen.unit(1))(1 ==)
    Prop.forAll(smallInts)(_.abs < 11)
    SProp.forAll(Gen.string(_, "123"))(s => "[123]*".r.unapplySeq(s) ? true | false)
    SProp.forAll(Gen.listOfN(_, smallNonNegative)) { xs =>
      val max = xs.max
      !xs.exists(_ > max)
    }
    /*
    val fixedNoneGreater = Prop.forAll(Gen.listOfN(5, smallInts)) { xs =>
      val max = xs.max
      !xs.exists(_ > max)
    }
    Prop.run(fixedNoneGreater)

    val noneGreater = SProp.forAll(SGen.listOf(smallInts)) { xs =>
      val max = xs.max
      !xs.exists(_ > max)
    }
    SProp.run(noneGreater)
    */
  }
}
