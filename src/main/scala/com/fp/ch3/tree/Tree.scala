package com.fp.ch3.tree

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maxInt(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(left, right) => {
        val maxLeft = maxInt(left)
        val maxRight = maxInt(right)
        if (maxLeft > maxRight) maxLeft else maxRight
      }
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => {
        val depthLeft = depth(left)
        val depthRight = depth(right)
        if (depthLeft > depthRight) depthLeft + 1 else depthRight + 1
      }
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](t: Tree[A], z: B)(f: (Tree[A], B) => B): B =
    t match {
      case Leaf(a) => f(Leaf(a), z)
      case Branch(left, right) => f(Branch(left, right), fold(right, f(left, fold(left, z)(f)))(f))
    }

  def fold2[A, B](t: Tree[A])(g: A => B, f: (B, B) => B): B =
    t match {
      case Leaf(a) => g(a)
      case Branch(left, right) => f(fold2(left)(g, f), fold2(right)(g, f))
    }

  def sizeFold(t: Tree[Int]): Int =
    fold2[Int, Int](t)(
      _ => 1,
      1 + _ + _)

  def maxIntFold(t: Tree[Int]): Int =
    fold2(t)(
      x => x,
      (x1: Int, x2: Int) => if (x1 > x2) x1 else x2)

  def depthFold[A](t: Tree[A]): Int =
    fold2[A, Int](t)(
      _ => 0,
      (d1, d2) => if (d1 > d2) d1 + 1 else d2 + 1)

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold2[A,Tree[B]](t)(
      a => Leaf(f(a)),
      (left, right) => Branch(left, right))
}

sealed trait T[+A] {
  def map[B](f: A => B): T[B] =
    this match {
      case Nil => Nil
      case Node(value, left, right) => Node(f(value), left.map(f), right.map(f))
    }

  def fold[B](z: B)(f: (A, B, B) => B): B =
    this match {
      case Nil => z
      case Node(value, left, right) => f(value, left.fold(z)(f), right.fold(z)(f))
    }
}
case class Node[+A](value: A, left: T[A], right: T[A]) extends T[A]
case object Nil extends T[Nothing]
