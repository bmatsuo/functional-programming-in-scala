package com.fp.ch8

import scalaz._
import Scalaz._

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
  type SuccessCount = Int
  type FailedCase = String

  def and(a: EitherProp, b: EitherProp): EitherProp = new EitherProp {
    def check = {
      a.check match {
        case left => left
        case Right(n) => b.check match {
          case Right(m) => Right(n + m)
          case Left((msg, m)) => Left((msg, n + m))
        }
      }
    }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type Result = Option[(FailedCase, SuccessCount)]
  type TestCases = Int

  /*
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    new Prop((n: TestCases) =>
      a.take(n)
        .map(as =>
          as.foldLeft(Right(0): Either[(FailedCase, SuccessCount), SuccessCount]){ (r, _a) =>
            r match {
              case Right(n) => if (f(_a)) Right(n+1) else Left((s"failed: $_a", n))
              case left => left
            }
          }
          .left
          .toOption))
          */
}

case class Prop(run: Prop.TestCases => Prop.Result) {
}

