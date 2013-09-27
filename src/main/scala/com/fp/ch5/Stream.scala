package com.fp.ch5

sealed abstract class Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B

  def uncons: Option[Cons[A]] =
    foldRight[Option[Cons[A]]](None)((a, c) => Some(Stream.mkCons(a, c getOrElse Empty)))

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = foldRight[List[A]](Nil)(_ :: _)

  def take(n: Int): Stream[A] =
    uncons match {
      case Some(c) if n > 0 => Stream.cons(c.head, c.tail.take(n-1))
      case _                 => Empty
    }

  def takeWhileNaive(f: A => Boolean): Stream[A] =
    uncons match {
      case Some(c) if f(c.head) => Stream.cons(c.head, c.tail.takeWhile(f))
      case _                    => Empty
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => b || p(a))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => b && p(a))

  def skip(n: Int): Stream[A] =
    uncons match {
      case Some(c) if n > 0 => c.tail.skip(n-1)
      case Some(c)          => c
      case _                => Empty
    }


  def takeWhile(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, as) =>
      if (f(a)) Stream.cons(a, as)
      else Empty)

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a, bs) => Stream.cons(f(a), bs))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, as) =>
      if (p(a)) Stream.cons(a, as)
      else as)

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((b, bs) => Stream.cons(b, bs))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((a, bs) => f(a).append(bs))
}

object Empty extends Stream[Nothing] {
  def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z
}

sealed abstract class Cons[+A] extends Stream[A] {
  def head: A
  def tail: Stream[A]

  def foldRight[B](z: => B)(f: (A, => B) => B): B = f(head, tail.foldRight(z)(f))
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = mkCons(h, t)

  private def mkCons[A](h: => A, t: => Stream[A]): Cons[A] = new Cons[A] {
    lazy val head = h
    lazy val tail = t
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))

  def flatten[A](streams: Stream[Stream[A]]): Stream[A] =
    streams.foldRight[Stream[A]](Empty)((s, ss) => s.append(ss))

  // rewriting this with unfold is fucking stupid
  // should be: cons(a, constant(a))
  def constant[A](a: A): Stream[A] = unfold(())(_ => Some(a, ()))

  // rewriting this with unfold is fucking stupid
  // should be: constant(1)
  val ones: Stream[Int] = unfold(())(_ => Some((1, ())))

  def from(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  val fibs: Stream[Int] =
    unfold((0, 1))(f => Some(f._1, (f._2, f._1 + f._2)))

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
    f(s) match {
      case Some((a, snew)) => Stream.cons(a, unfold(snew)(f))
      case None            => Empty
    }
}

