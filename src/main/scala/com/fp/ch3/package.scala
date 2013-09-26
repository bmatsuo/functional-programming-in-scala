package com.fp

import scala.annotation.tailrec

package object ch3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1
      case Cons(x, xs) => x * product(xs)
    }

    // This is kind of bullshit because it bootstraps on Seq
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // Exercise 1: x == 3
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(a, Cons(2, Cons(4, _))) => a
      case Nil => 42
      case Cons(a, Cons(b, Cons(3, Cons(4, _)))) => a + b
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    // Exercise 2
    def tail[A](lis: List[A]): List[A] = lis match {
      case Nil => throw new RuntimeException("tail of Nil is not defined")
      case Cons(_, t) => t
    }

    // Exercise 3
    // the typing appears to be important. contravariant vs covariant types.
    // B is a 'more general' A (a superclass)
    def setHead[A](lis: List[A], a: A): List[A] = lis match {
      case Nil => throw new RuntimeException("setHead of Nil is not defined")
      case Cons(_, t) => Cons(a, t)
    }

    // Exercise 4
    @tailrec
    def drop[A](as: List[A], n: Int): List[A] =
      if (n < 0)
        throw new RuntimeException("drop given a negative number")
      else if (n == 0)
        as
      else as match {
        case Nil => throw new RuntimeException("drop")
        case Cons(_, as) => drop(as, n-1)
      }

    // Exercise 5
    @tailrec
    def dropWhile[A](as: List[A], cont: A => Boolean): List[A] = as match {
      case Cons(x, xs) if cont(x) => dropWhile(xs, cont)
      case _ => as
    }

    // Exercise 6: order n unlike `tail`
    def init[A](as: List[A]): List[A] = as match {
      case Nil => throw new RuntimeException("init of Nil is not defined")
      case Cons(a, Nil) => Nil
      case Cons(a, as) => Cons(a, init(as))
    }

    // foldLeft(Cons(a, Cons(b, Cons(c, Nil))), z)(f)
    // f(a, foldLeft(Cons(b, Cons(c, Nil)), z)(f))
    // f(a, f(b, foldLeft(Cons(c, Nil), z)(f)))
    // f(a, f(b, f(c, foldLeft(Nil, z)(f))))
    // f(a, f(b, f(c, z)))
    def foldRight[A, B](lis: List[A], z: B)(f: (A, B) => B): B =
      lis match {
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }

    def sum2(lis: List[Int]): Int =
      foldRight(lis, 0)(_ + _)

    def product2(lis: List[Double]): Double =
      foldRight(lis, 1.0)(_ * _)

    def length[A](lis: List[A]): Int =
      foldRight(lis, 0)((_, b) => b + 1)

    // foldLeft(Cons(a, Cons(b, Cons(c, Nil))), z)(f)
    // foldLeft(Cons(b, Cons(c, Nil)), f(z, a))(f)
    // foldLeft(Cons(c, Nil), f(f(z, a), b))(f)
    // foldLeft(Nil, f(f(f(z, a), b), c))(f)
    // f(f(f(z, a), b), c)
    @tailrec
    def foldLeft[A, B](lis: List[A], z: B)(f: (B, A) => B): B =
      lis match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }

    def sumLeft(lis: List[Int]): Int =
      foldLeft(lis, 0)(_ + _)

    def productLeft(lis: List[Double]): Double =
      foldLeft(lis, 1.0)(_ * _)

    def lengthLeft[A](lis: List[A]): Int =
      foldLeft(lis, 1)((count, _) => count + 1)

    def reverse[A](lis: List[A]): List[A] =
      foldLeft(lis, Nil: List[A])((rev, h) => Cons(h, rev))

    def concat[A](lis1: List[A], lis2: List[A]): List[A] =
      foldRight(lis1, lis2)(Cons(_, _))

    def append[A](lis: List[A], a: A): List[A] =
      concat(lis, List(a))

    def flatten[A](lis: List[List[A]]): List[A] =
      foldRight(lis, Nil: List[A])(concat)

    def foldRight2[A, B](lis: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(lis), z)((b, a) => f(a, b))

    def incrAll(lis: List[Int]): List[Int] =
      foldRight(lis, Nil: List[Int])((i, t) => Cons(i + 1, t))

    def doubleStrings(lis: List[Double]): List[String] =
      foldRight(lis, Nil: List[String])((d, t) => Cons(d.toString, t))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

    def filter[A](lis: List[A])(f: A => Boolean): List[A] =
      foldRight(lis, Nil: List[A]) { (a, as) =>
        if (f(a)) Cons(a, as)
        else as
      }

    def flatMap[A, B](lis: List[A])(f: A => List[B]): List[B] =
      flatten(map(lis)(f))

    def filter2[A](lis: List[A])(f: A => Boolean): List[A] =
      flatMap(lis) { a => if (f(a)) List(a) else Nil }

    def zipAdd(lis1: List[Int], lis2: List[Int]): List[Int] =
      (lis1, lis2) match {
        case (_, Nil) => lis1
        case (Nil, _) => lis2
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, zipAdd(t1, t2))
      }

    def zip[A, B](lis1: List[A], lis2: List[A])(f: (A, A) => B): List[B] =
      (lis1, lis2) match {
        case (Nil, Nil) => Nil
        case (_, Nil) => throw new RuntimeException("zip")
        case (Nil, _) => throw new RuntimeException("shit")
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zip(lis1, lis2)(f))
      }

    def hasSubsequence[A](lis: List[A], sub: List[A]): Boolean = {
      @tailrec
      def startsWith(lis: List[A], sub: List[A]): Boolean =
        (lis, sub) match {
          case (_, Nil) => true
          case (Nil, _) => false
          case (Cons(hlis, tlis), Cons(hsub, tsub)) if hlis == hsub => startsWith(tlis, tsub)
          case _ => false
        }

      @tailrec
      def hasSub(lis: List[A]): Boolean =
        if (startsWith(lis, sub)) true
        else lis match {
          case Nil => false
          case Cons(_, tlis) => hasSub(tlis)
        }

      hasSub(lis)
    }

  }
}
