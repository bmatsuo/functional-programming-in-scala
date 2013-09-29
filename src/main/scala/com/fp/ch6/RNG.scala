package com.fp.ch6

import com.fp.ch5._


trait RNG {
  def nextInt: (Int, RNG)

  def positiveInt: (Int, RNG) =
    nextInt match {
      case (x, r) if x == Int.MinValue  => r.positiveInt
      case (x, r) if x == 0             => r.positiveInt
      case (x, r)                       => (math.abs(x), r)
    }

  def postiveEven: (Int, RNG) = RNG.positiveEven(this)

  def double: (Double, RNG) = RNG.double(this)

  def intDouble: ((Int, Double), RNG) = {
    val (n, r) = nextInt
    val (x, r2) = r.double
    ((n, x), r2)
  }

  def doubleInt: ((Double, Int), RNG) = {
    val (x, r) = double
    val (n, r2) = r.nextInt
    ((x, n), r2)
  }

  def doubleDoubleDouble: ((Double, Double, Double), RNG) = {
    val (x, r) = double
    val (x2, r2) = r.double
    val (x3, r3) = r2.double
    ((x, x2, x3), r3)
  }

  def ints(count: Int): (List[Int], RNG) =
    if (count <= 0)
      (Nil, this)
    else {
      val (x, r) = nextInt
      val (xs, r2) = r.ints(count - 1)
      (x :: xs, r2)
    }
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed*0x5DEECE66DL +  0xBL) & ((1L << 48) - 1)
      ((seed >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] =
    State(_.nextInt)

  val positiveInt: Rand[Int] =
    State(_.positiveInt)

  def unit[A](a: A): Rand[A] =
    State((a, _))

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    s.map(f)

  val positiveEven: Rand[Int] =
    map(positiveInt)(i => i - i % 2)

  val double: Rand[Double] =
    map(positiveInt)(n => (n.toDouble - 1) / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    ra.map2(rb)(f)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    ra.map2(rb)((_, _))

  val intDouble: Rand[(Int, Double)] = both(int, double)

  val doubleInt: Rand[(Double, Int)] = both(double, int)

  val doubleDoubleDouble: Rand[(Double, Double, Double)] =
    flatMap(double)(x1 =>
      flatMap(double)(x2 =>
        map(double)(x3 => (x1, x2, x3))))

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    State.sequence(ras)

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](ra: Rand[A])(g: A => Rand[B]): Rand[B] =
    ra.flatMap(g)

  def positiveLessThan(n: Int): Rand[Int] =
    flatMap(positiveInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod > 0) unit(mod) else positiveLessThan(n)
    }

  def mapFlat[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2Flat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def map2For[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    for {
      a <- ra
      b <- rb
    } yield f(a, b)

  val rollDie: Rand[Int] = positiveLessThan(6)

  val rollDieCorrect: Rand[Int] = map(positiveLessThan(6))(_ + 1)
}
