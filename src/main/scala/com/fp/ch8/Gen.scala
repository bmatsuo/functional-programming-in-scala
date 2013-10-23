package com.fp.ch8

import scalaz._
import Scalaz._

import com.fp.ch6._

case class Gen[A](sample: RNG.Rand[A]) {
  import Gen._

  def map[B](f: A => B): Gen[B] =
    Gen(RNG.map(sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(RNG.flatMap(sample)(f(_).sample))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    for {
      a <- this
      b <- b
    }
    yield f(a, b)

  def filter(f: A => Boolean): Gen[A] =
    flatMap(a => f(a) ? unit(a) | filter(f))

  def toSGen: SGen[A] = SGen(_ => this)

  def take(n: Int): Gen[List[A]] =
    sequence(Stream
      .continually(this)
      .take(n)
      .toList)
}

object Gen {
  def unit[A](a: => A): Gen[A] = // XXX why is the argument unevaluated?
    Gen(RNG.unit(a))

  def int: Gen[Int] =
    Gen(RNG.int)

  def char(alpha: String): Gen[Char] =
    choose(alpha.toList)

  def boolean: Gen[Boolean] =
    choose(List(true, false))

  def string(n: Int, alpha: String): Gen[String] =
    char(alpha) take n map (_.mkString)

  def stream[A](a: Gen[A])(rng: RNG): Stream[A] =
    Stream.iterate[(A, RNG)](a sample rng)(x => a.sample(x._2)).map(_._1)

  def positiveLessThan(n: Int): Gen[Int] =
    int.flatMap({
      case Int.MinValue => positiveLessThan(n)
      case m            => unit(m.abs)
    })

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    positiveLessThan(stopExclusive - start) map (start +)

  def choose[A](a: Array[A]): Gen[A] =
    choose(0, a.length) map a

  def choose[A](a: List[A]): Gen[A] =
    choose(0, a.length) map a

  def choose[A, B](m: Map[A, B]): Gen[B] =
    choose(m.keys.toList) map m

  def pairRange(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    pair(choose(start, stopExclusive), choose(start, stopExclusive))

  def sequence[A](lis: List[Gen[A]]): Gen[List[A]] =
    lis.foldRight(unit[List[A]](Nil)) { (ga, gas) =>
      for {
        a <- ga
        as <- gas
      } yield a :: as
    }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    g take n

  def pair[A, B](a: Gen[A], b: Gen[B]): Gen[(A, B)] =
    (a map2 b)((_, _))

  def triple[A, B, C](a: Gen[A], b: Gen[B], c: Gen[C]): Gen[(A, B, C)] =
    for {
      a <- a
      b <- b
      c <- c
    } yield (a, b, c)

  def filterOption[A](a: Gen[Option[A]]): Gen[A] =
    a flatMap { _.map(unit(_)).getOrElse(filterOption(a)) }

  def option[A](a: Gen[A])(select: A => Boolean): Gen[Option[A]] =
    a map { a => select(a).option(a) }

  def some[A](a: Gen[A]): Gen[Option[A]] =
    option(a)(_ => true)

  // pattern(unit("def _"), string(20, "abcdefghijklmnopqrstuvwxyz"), unit("(x: String) { println(x) }"))
  def pattern(s: Gen[String]*): Gen[String] =
    sequence(s.toList) map (_.mkString)

  /*
  implicit class EnrichedStringGen(s: Gen[String]) {
    //    string(20, "0123456789abcdef") ~ ": " ~ choose(List("String", "Int", "Double"))
    def ~(that: EnrichedStringGen): EnrichedStringGen =
      new EnrichedStringGen(pattern(s, that.s))
  }

  import scala.language.implicitConversions
  implicit def enrichString(s: String): EnrichedStringGen =
    new EnrichedStringGen(unit(s))

  def $: EnrichedStringGen = enrichString("")
  */

  def union[A](a1: Gen[A], a2: Gen[A]): Gen[A] =
    choose(List(a1, a2)) flatMap identity

  /*
  def weighted[A](a1: (Gen[A], Double), a2: (Gen[A], Double)): Gen[A] = {
    val p1 = a1._2 / (a1._2 + a2._2)
    double flatMap { if (_ < p1) a1._1 else  a2._1}
  }
  */
}
