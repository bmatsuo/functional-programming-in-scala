package com.fp.ch4

import com.fp.common._

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    flatMap(a => Right(f(a)))

  def map2[F >: E, B, C](b: Either[F, B])(f: (A, B) => C): Either[F, C] =
    flatMap(a => b.map(f(a, _)))

  def flatMap[F >: E, B](f: A => Either[F, B]) =
    this match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }

  def orElse[F >: E, B >: A](b: Either[F, B]): Either[F, B] =
    this match {
      case Right(a) => Right(a)
      case _        => b
    }
}

case class Left[E](value: E) extends Either[E, Nothing]
case class Right[A](value: A) extends Either[Nothing, A]

object Either {
  def traverse[A, E, B](as: List[A], f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))( (a, _bs) =>
      _bs.flatMap( bs => f(a).map(_ :: bs)))

  def sequence[A, E](lis: List[Either[E, A]]): Either[E, List[A]] =
    traverse[Either[E, A], E, A](lis, identity)
}
