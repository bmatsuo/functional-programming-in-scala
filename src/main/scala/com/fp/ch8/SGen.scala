package com.fp.ch8

case class SGen[A](forSize: Int => Gen[A]) {
  def mod[B](f: Gen[A] => Gen[B]): SGen[B] =
    SGen(n => f(forSize(n)))

  def map[B](f: A => B): SGen[B] =
    mod(_ map f)

  // def flatMap[B](f: A => SGen[B]): SGen[B] = ???

  def filter(f: A => Boolean): SGen[A] =
    mod(_ filter f)

  def take(n: Int): SGen[List[A]] =
    mod(_ take n)
}

object SGen {
  // FIXME unable to define as a method on SGen. weird type stuff?
  def map2[A, B, C](a: SGen[A], b: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen { n =>
      val _a = a.forSize(n)
      val _b = b.forSize(n)
      (_a map2 _b)(f)
    }

  def listOf[A](a: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, a))
}
