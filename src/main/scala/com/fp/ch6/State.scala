package com.fp.ch6

case class State[S, +A](action: S => (A, S)) {
  def apply(s: S): (A, S) = action(s)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S,B](s1 => {
      val (a, s2) = apply(s1)
      f(a)(s2)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def map2For[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)
}

object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    State[S, List[A]](s =>
      sas.foldRight[(List[A], S)]((Nil, s))({ case (sa, (as, s1)) =>
        val (a, s2) = sa.action(s1)
        (a :: as, s2)
      }))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def nop[S, A]: State[S, Unit] = modify(identity)
}

