package com.kczulko.chapter3.datastructures


sealed trait BinTree[+A]

case class Leaf[A](value: A) extends BinTree[A]
case class Branch[A](left: BinTree[A], right: BinTree[A]) extends BinTree[A]

object BinTree {
  def size[A](binTree: BinTree[A]): Int = fold(binTree, (a : A) => 1)((x,y) => 1 + x() + y())

  def maximum(binTree: BinTree[Int]): Int = fold(binTree, (a: Int) => a)(_() max _())

  def depth[A](binTree: BinTree[A]): Int =
    fold(binTree, (a: A) => 1)((x,y) => (x() + 1).max(y() + 1))

  def map[A,B](binTree: BinTree[A])(f: A => B): BinTree[B] =
    fold(binTree, (a: A) => Leaf(f(a)): BinTree[B])((x,y) => Branch(x(), y()))

  def fold[A,B](binTree: BinTree[A], b: A => B)(f: (() => B, () => B) => B): B = binTree match {
    case Leaf(v) => b(v)
    case Branch(l, r) => f(() => fold(l,b)(f), () => fold(r,b)(f))
  }
}

