import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.fp.ch5._
import com.fp.ch6._

class Ch6 extends FunSuite with Matchers {
  import RNG._

  test("x.x: simple algorithm check") {
    pending
    def sample(n: Int, seed: Long): Stream[Int] = Stream
      .unfold(simple(seed))(r => Some(r.nextInt))
      .take(n)

    sample(3, 42).toList should be (List(0, 16159453, -1281479697))
  }

  test("6.1: postiveInt") {
    pending
    def sample(n: Int, seed: Long): Stream[Int] = Stream
      .unfold(simple(seed))(r => Some(r.positiveInt))
      .take(n)

    sample(2, 42).toList should be (List(16159453, 1281479697))
    Range(1, 100).forall(sample(1000, _).forAll(_ > 0)) should be (true)
  }

  test("6.2: double") {
    pending
    def sample(n: Int, seed: Long): Stream[Double] = Stream
      .unfold(simple(seed))(r => Some(r.double))
      .take(n)

    Range(1, 100).forall(sample(1000, _).forAll(x => 1.0 > x && x >= 0.0)) should be (true)
  }

  test("6.8: flatMap and postiveLessThan") {
    pending
    def sample(n: Int, lessThan: Int, seed: Long): Stream[Int] = Stream
      .unfold(simple(seed))(r => Some(RNG.positiveLessThan(lessThan)(r)))
      .take(n)

    val max = 1281479697
    Range(1, 100).forall(sample(1000, max, _).forAll(x => max > x && x > 0)) should be (true)
  }

  test("x.x: reproducible errors") {
    pending
    rollDie(simple(5)) should be (0, _: RNG)
  }

  sealed trait Input

  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def simulate(input: Input): Machine =
      if (candies <= 0) this
      else (input, locked) match {
        case (Turn, true)  => this
        case (Turn, false) => Machine(true, candies-1, coins)
        case (Coin, false) => this
        case (Coin, true)  => Machine(false, candies, coins+1)
      }
  }

  object Machine {
    type MachineState = State[Machine, (Int, Int)]

    private def step(input: Input): State[Machine, Unit] =
      State.modify[Machine](_.simulate(input))

    def simulate(inputs: List[Input]): State[Machine, (Int, Int)] =
      State(m => {
        val m1 = inputs
          .foldLeft[State[Machine, Unit]](State.nop)((s, in) => for {
            _ <- s
            _ <- step(in)
          } yield ())
          .apply(m)._2

        ((m1.coins, m1.candies), m1)
      })
  }

  test("6.11: simulate") {
    {
      pending
    }
    {
      val m1 = Machine(true, 5, 10)
      val inputs = List(
        Coin, Turn,
        Coin, Turn,
        Coin, Turn,
        Coin, Turn)
      Machine.simulate(inputs)(m1) should be ((14, 1), _: Machine)
    }
    {
      val m1 = Machine(true, 5, 10)
      val inputs = List(
        Coin, Coin, Turn, Turn,
        Coin, Coin, Turn, Turn,
        Coin, Coin, Turn, Turn,
        Coin, Coin, Turn, Turn)
      Machine.simulate(inputs)(m1) should be ((14, 1), _: Machine)
    }
  }
}
