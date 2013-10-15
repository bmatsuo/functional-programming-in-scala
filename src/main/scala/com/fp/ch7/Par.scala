package com.fp.ch7


import java.util.concurrent.{Future, Callable, TimeUnit, ExecutorService}

import scalaz._
import Scalaz._
import scalaz.syntax.std._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def nop: Par[Unit] = unit(Unit)

  def unit[A](a: A): Par[A] =
    _ => new Future[A] {
      def get: A = a
      def get(timeout: Long, unit: TimeUnit): A = a
      def cancel(b: Boolean): Boolean = false
      val isCancelled = false
      val isDone = true
    }

  def fork[A](a: => Par[A]): Par[A] =
    s => s.submit(new Callable[A] {
      def call: A =
        run(s)(a).get // java future insulates exceptions
    })

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    fork(es => {
      val _a = run(es)(a).get
      run(es)(f(_a))
    })
  }

  def join[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  def async[A](a: => A): Par[A] = fork(unit(a))

  def chooser[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    flatMap(a)(f)

  def choiceN[A](i: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(i)(choices)

  def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] =
    chooser(a)(choices)

  def choice[A](b: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    chooser(b)(_ ? ifTrue | ifFalse)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    flatMap(a)(a =>
      flatMap(b)(b =>
        unit(f(a, b))))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => async(f(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def sortNaive(l: Par[List[Int]]): Par[List[Int]] =
    map2(l, nop)((a, _) => a.sorted)

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, nop)((a, _) => f(a))

  def sort(l: Par[List[Int]]): Par[List[Int]] =
    map(l)(_.sorted)

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(Nil: List[A])) { map2(_, _)(_ :: _) }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(l.map(asyncF(f))))

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    // filter with Option type, then flatten.
    val opts = parMap(l)(a => f(a).option(a))
    map(opts)(_.flatten)
  }

  def parFoldRight[A, B](l: List[A], z: B)(f: (A, B) => B ): Par[B] =
    l.foldRight[Par[B]](unit(z)) { (a, b) => map(b)(f(a, _)) }

  def parFoldLeft[A, B](l: List[A], z: B)(f: (B, A) => B ): Par[B] =
    l.foldLeft[Par[B]](unit(z)) { (b, a) => map(b)(f(_, a)) }
}
