package com.fp.ch3

sealed trait Tree[+A] {
  import Tree._

  def size: Int =
    this match {
      case Branch(left, right) => 1 + left.size + right.size
      case Leaf(_)             => 1
    }

  def depth: Int =
    this match {
      case Branch(left, right) =>  max(left.depth, right.depth) + 1
      case Leaf(_)             => 0
    }

  def map[B](f: A => B): Tree[B] =
    this match {
      case Branch(left, right) => Branch(left.map(f), right.map(f))
      case Leaf(a)             => Leaf(f(a))
    }

  def fold[B](g: A => B, f: (B, B) => B): B =
    this match {
      case Branch(left, right) => f(left.fold(g, f), right.fold(g, f))
      case Leaf(a)             => g(a)
    }

  def sizeFold: Int =
    fold[Int](
      _ => 1,
      1 + _ + _)

  def depthFold: Int =
    fold[Int](
      _ => 0,
      max(_, _) + 1)

  def mapFold[B](f: A => B): Tree[B] =
    fold[Tree[B]](a => Leaf(f(a)), Branch.apply)
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  private def max2(x: Int, y: Int): Int = if (x > y) x else y
  private def max(x: Int, xs: Int*): Int = xs.foldLeft(x)(max2)

  def maxInt(t: Tree[Int]): Int =
    t match {
      case Branch(left, right) => max(maxInt(left), maxInt(right))
      case Leaf(x)             => x
    }

  def maxIntFold(t: Tree[Int]): Int =
    t.fold[Int](identity, max(_, _))
}
