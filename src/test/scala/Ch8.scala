import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.fp.ch8._

class Ch8 extends FunSuite with Matchers {
  test("8.3: BoolProp") {
    def pass = new BoolProp { def check = true }
    def fail = new BoolProp { def check = false }
    (pass && pass).check should be (true)
    (pass && fail).check should be (false)
    (fail && pass).check should be (false)
    (fail && fail).check should be (false)
  }
}
