package com.fp.ch3

import scala.annotation.tailrec

sealed trait List[+A] {
  // Exercise 2
  def tail: List[A] = this match {
    case Cons(_, t) => t
    case Nil        => throw new RuntimeException("tail of Nil is not defined")
  }

  // Exercise 3
  // the typing appears to be important. contravariant vs covariant types.
  // B is a 'more general' A (a superclass)
  def setHead[B >: A](b: B): List[B] =
    this match {
      case Cons(_, t) => Cons(b, t)
      case Nil        => Nil
    }

  // Exercise 4
  @tailrec
  final def drop(n: Int): List[A] =
    if (n < 0) throw new RuntimeException("drop given a negative number")
    else if (n == 0) this
    else this match {
      case Cons(_, t) => t.drop(n-1)
      case Nil        => Nil
    }

  // Exercise 5
  @tailrec
  final def dropWhile(cont: A => Boolean): List[A] = this match {
    case Cons(x, xs)
      if cont(x) => xs.dropWhile(cont)
    case _       => this
  }

  // Exercise 6: order n unlike `tail`
  def init: List[A] = this match {
    case Cons(a, Nil) => Nil
      println(s"bot $this")
      Nil
    case Cons(a, Cons(b, t))  =>
      println(s"rec $this")
      val _init = Cons(a, Cons(b, t).init)
      println(s"bld ${_init}")
      _init
    case Nil          => throw new RuntimeException("init of Nil is not defined")
  }

  // foldRight(List(a, b, c)), z)(f)
  // f(a, foldRight(List(b, c), z)(f))
  // f(a, f(b, foldRight(List(c), z)(f)))
  // f(a, f(b, f(c, foldRight(Nil, z)(f))))
  // f(a, f(b, f(c, z)))
  def foldRight[B](z: B)(f: (A, B) => B): B =
    this match {
      case Nil        => z
      case Cons(h, t) => f(h, t.foldRight(z)(f))
    }

  def length: Int =
    foldRight(0)((_, b) => b + 1)

  // foldLeft(List(a, b, c), z)(f)
  // foldLeft(List(b, c), f(z, a))(f)
  // foldLeft(List(c), f(f(z, a), b))(f)
  // foldLeft(Nil, f(f(f(z, a), b), c))(f)
  // f(f(f(z, a), b), c)
  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match {
      case Cons(h, t) => t.foldLeft(f(z, h))(f)
      case Nil        => z
    }

  def lengthLeft: Int =
    foldLeft(1)((count, _) => count + 1)

  def reverse: List[A] =
    foldLeft(Nil: List[A])((rev, h) => Cons(h, rev))

  def concat[B >: A](bs: List[B]): List[B] =
    foldRight(bs)(Cons(_, _))

  def append[B >: A](b: B): List[B] =
    concat(List(b))

  def foldRight2[B](z: B)(f: (A, B) => B): B =
    reverse.foldLeft(z)((b, a) => f(a, b))

  def map[B](f: A => B): List[B] =
    foldRight(Nil: List[B])((a, bs) => Cons(f(a), bs))

  def filter(f: A => Boolean): List[A] =
    foldRight(Nil: List[A]) { (a, as) =>
      if (f(a)) Cons(a, as)
      else as
    }

  def flatMap[B](f: A => List[B]): List[B] =
    List.flatten[B](this.map(f))

  def filter2(f: A => Boolean): List[A] =
    flatMap { a => if (f(a)) List(a) else Nil }

  def zip[B, C](lis2: List[B])(f: (A, B) => C): List[C] =
    (this, lis2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), t1.zip(t2)(f))
      case _                            => Nil
    }

  def hasSubsequence[B >: A](sub: List[B]): Boolean = {
    @tailrec
    def startsWith(lis: List[A], sub: List[B]): Boolean =
      (lis, sub) match {
        case (Cons(h, t), Cons(hsub, tsub))
          if h == hsub => startsWith(t, tsub)
        case (_, Nil)  => true
        case (Nil, _)  => false
        case _         => false
      }

    @tailrec
    def hasSub(lis: List[A]): Boolean =
      if (startsWith(lis, sub)) true
      else lis match {
        case Cons(_, t) => hasSub(t)
        case Nil        => false
      }

    hasSub(this)
  }
}
case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Cons(x, xs) => x + sum(xs)
    case Nil         => 0
  }

  def product(ds: List[Double]): Double = ds match {
    case Cons(x, xs) => x * product(xs)
    case Nil         => 1
  }

  // This is kind of bullshit because it bootstraps on Seq
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 1: x == 3
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(a, Cons(2, Cons(4, _)))          => a
    case Nil                                   => 42
    case Cons(a, Cons(b, Cons(3, Cons(4, _)))) => a + b
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def sum2(lis: List[Int]): Int =
    lis.foldRight(0)(_ + _)

  def product2(lis: List[Double]): Double =
    lis.foldRight(1.0)(_ * _)

  def sumLeft(lis: List[Int]): Int =
    lis.foldLeft[Int](0)(_ + _)

  def productLeft(lis: List[Double]): Double =
    lis.foldLeft[Double](1.0)(_ * _)

  def flatten[A](lis: List[List[A]]): List[A] =
    lis.foldRight(Nil: List[A])(_ concat _)

  def incrAll(lis: List[Int]): List[Int] =
    lis.foldRight(Nil: List[Int])((i, t) => Cons(i + 1, t))

  def doubleStrings(lis: List[Double]): List[String] =
    lis.foldRight(Nil: List[String])((d, t) => Cons[String](s"$d", t))

  def zipAdd(lis1: List[Int], lis2: List[Int]): List[Int] =
    (lis1, lis2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, zipAdd(t1, t2))
      case (_, Nil)                     => Nil
      case (Nil, _)                     => Nil
    }
}
