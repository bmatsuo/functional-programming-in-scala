package com.fp.ch4

import com.fp.common._
import scala.math.{sqrt, pow}
import java.util.regex._

sealed trait Option[+A] {
  private def make[B](f: A => B, g: => B): B = this match {
    case Some(a) => f(a)
    case _ => g
  }

  def map[B](f: A => B): Option[B] =
    make(a => Some(f(a)), None)

  def flatMap[B](f: A => Option[B]): Option[B] =
    make(f, None)

  def getOrElse[B >: A](default: B): B =
    make(identity, default)

  def orElse[B >: A](default: => B): Option[B] =
    make(a => Some(a), Some(default))

  def filter(f: A => Boolean): Option[A] =
    make(a => if (f(a)) Some(a) else None, None)
}

case class Some[A](value: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs)
      .map(µ => xs.map(x => pow(x - µ, 2.0)))
      .flatMap(mean)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B], f: (A, B) => C): Option[C] =
    for {
      a <- a
      b <- b
    } yield f(a,b)

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat:String): Option[String => Boolean] =
    pattern(pat) map (p => s => p.matcher(s).matches)

  def bothMatchNaive(pat1: String, pat2: String, s: String): Option[Boolean] =
    for {
      m1 <- mkMatcher(pat1)
      m2 <- mkMatcher(pat2)
    } yield m1(s) && m2(s)

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2[String => Boolean, String => Boolean, Boolean](
      mkMatcher(pat1),
      mkMatcher(pat1),
      _(s) && _(s))

  def cons[A](a: Option[A], as: Option[List[A]]): Option[List[A]] =
    map2[A, List[A], List[A]](a, as, _ :: _)

  def sequenceNaive[A](lis: List[Option[A]]): Option[List[A]] =
    lis.foldRight[Option[List[A]]](Some(Nil))(cons)

  def traverse[A,B](as: List[A], f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, _bs) => cons(f(a), _bs) )

  def sequence[A](lis: List[Option[A]]): Option[List[A]] =
    traverse[Option[A], A](lis, identity)
}

