package com.fp

import annotation.tailrec

package object ch2 {
  // Exercise 1
  def fib(n: Int): Int = {
    @tailrec
    def _fib(i: Int, n: Int, acc1: Int, acc2: Int): Int =
      if (i >= n) acc1 else _fib(i+1, n, acc1 + acc2, acc1)

    if (n < 1)
      0
    else if (n == 1)
      1
    else
      _fib(1, n, 1, 0)
  }

  // Exercise 2
  @tailrec
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    if (as.length < 2)
      true
    else if (gt(as(0), as(1)))
      false
    else
      isSorted(as.slice(1, as.length), gt)
  }

  // Exercise 2: with case classes
  @tailrec
  def isSortedList[A](as: List[A], gt: (A, A) => Boolean): Boolean =
    as match {
      case x :: y :: rest if gt(x, y) => false
      case x :: y :: rest             => isSortedList(y :: rest, gt)
      case _                          => true
    }

  // Exercise 3
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  // Exercise 4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // Exercise 5
  def compose[A, B, C](g: B => C, f: A => B): A => C =
    (a: A) => g(f(a))
}
