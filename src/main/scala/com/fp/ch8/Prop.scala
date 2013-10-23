package com.fp.ch8

import scalaz._
import Scalaz._
import com.fp.ch6.RNG

trait BoolProp {
  import BoolProp._

  def check: Boolean

  def &&(p: BoolProp): BoolProp = and(this, p)
}

object BoolProp {
  def and(a: BoolProp, b: BoolProp): BoolProp = new BoolProp {
    def check = a.check && b.check
  }
}

trait EitherProp {
  import EitherProp._

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: EitherProp): EitherProp = and(this, p)
}

object EitherProp {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  def and(a: EitherProp, b: EitherProp): EitherProp = new EitherProp {
    def check = {
      a.check match {
        case Right(n) => b.check match {
          case Right(m) => Right(n + m)
          case Left((msg, m)) => Left((msg, n + m))
        }
        case left => left
      }
    }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type Result = Option[(FailedCase, SuccessCount)]
  type TestCases = Int

  def run(p: Prop,
          testCases: Int = 100,
          rng: RNG = RNG.simple(System.currentTimeMillis)): Result = {
    p.run(testCases, rng) match {
      case Some((msg, n)) =>
        println(s"! Falsified after $n passed tests:\n$msg")
        Some(msg, n)
      case None =>
        println(s"+ OK, passed $testCases tests.")
        None
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop({ (n, rng) =>
      val cases = Gen
        .stream(as)(rng)
        .zip(Stream.from(0))
        .take(n)

      cases.foldLeft[Result](None)({
        case (None, (a, i)) =>
          try {
            f(a) ? (None: Result) | Some(s"failed case: $a", i)
          } catch {
            case e: Exception =>
              val msg =
                s"test case: $a\n" +
                  s"generated an exception: ${e.getMessage}\n" +
                  s"stack trace\n\t${e.getStackTrace.mkString("\n\t")}\n"
              Some(msg, i)
          }
        case (some, _) => some
      })
    })

  def named(name: String, prop: Prop): Prop =
    Prop { (n, rng) =>
      for {
        (err, n) <- prop.run(n, rng)
      } yield (s"$name:\n\t${err.trim.replace("\n", "\n\t")}", n)
    }

  implicit class EnrichedString(s: String) {
    def :=(p: Prop): Prop =
      named(s, p)
  }
}

case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {
  def &&(that: Prop): Prop = Prop({ (n, rng) =>
    run(n, rng).orElse(that.run(n, rng))
  })

  def ||(that: Prop): Prop = Prop({ (n, rng) =>
    for {
      res1 <- this.run(n, rng)
      res2 <- that.run(n, rng)
      msg = s"${res1._1}\n${res2._1}"
      m = res1._2 max res2._2
    } yield (msg, m)
  })
}

object SProp {
  type SuccessCount = Int
  type FailedCase = String
  type Result = Option[(FailedCase, SuccessCount)]
  type TestCases = Int
  type MaxSize = Int

  def success(res: Result): Boolean = res.isEmpty
  def failure(res: Result): Boolean = !success(res)

  def run(p: SProp,
           maxSize: Int = 100,
           testCases: Int = 100,
           rng: RNG = RNG.simple(System.currentTimeMillis)): Result = {
    p.run(maxSize, testCases, rng) match {
      case Some((msg, n)) =>
        println(s"! Falsified after $n passed tests:\n$msg")
        Some(msg, n)
      case None =>
        println(s"+ OK, passed $testCases tests.")
        None
    }
  }

  def check(p: => Boolean): SProp =
    forAll(Gen.unit(Unit))(_ => p)

  def forAll[A](as: Gen[A])(f: A => Boolean): SProp =
    SProp({ (_, n, rng) =>
      val cases = Gen
        .stream(as)(rng)
        .zip(Stream.from(0))
        .take(n)

      cases.foldRight[Result](None)({
        case ((a, i), None) =>
          try {
            f(a) ? (None: Result) | Some(s"failed case: $a", i)
          } catch {
            case e: Exception =>
              val msg =
                s"test case: $a\n" +
                  s"generated an exception: ${e.getMessage}\n" +
                  s"stack trace\n\t${e.getStackTrace.mkString("\n\t")}\n"
              Some(msg, i)
          }
        case (_, some) => some
      })
    })

  def forAll[A](as: SGen[A])(f: A => Boolean): SProp =
    forAll(as.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): SProp =
    SProp({ (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max // divide cases evenly
      val props: Stream[SProp] =               // a lazy stream of fixed-size props
        Stream.from(0)
          .take(n min max)
          .map(i => forAll(g(i))(f))
      val prop: SProp =                        // conjunction of all fixed-size props
        props.map(p => SProp { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList
          .reduceLeft(_ && _)
      prop.run(max, n, rng)
    })

  def named(name: String, prop: SProp): SProp =
    SProp { (max, n, rng) =>
      for {
        (err, n) <- prop.run(max, n, rng)
      } yield (s"$name:\n\t${err.trim.replace("\n", "\n\t")}", n)
    }

  implicit class EnrichedString(s: String) {
    def :=(p: SProp): SProp =
      named(s, p)
  }
}

case class SProp(run: (SProp.MaxSize, SProp.TestCases, RNG) => SProp.Result) {
  def &&(that: SProp): SProp = SProp({ (max, n, rng) =>
    run(max, n, rng).orElse(that.run(max, n, rng))
  })

  def ||(that: SProp): SProp = SProp({ (max, n, rng) =>
    for {
      res1 <- this.run(max, n, rng)
      res2 <- that.run(max, n, rng)
      msg = s"${res1._1}\n${res2._1}"
      m = res1._2 max res2._2
    } yield (msg, m)
  })
}
