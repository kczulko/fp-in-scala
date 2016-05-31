package com.kczulko.chapter3.datastructures


sealed trait BinTree[+A]

case class Leaf[A](value: A) extends BinTree[A]
case class Branch[A](left: BinTree[A], right: BinTree[A]) extends BinTree[A]

object BinTree {
  def size[A](binTree: BinTree[A]): Int = binTree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(binTree: BinTree[Int]): Int = binTree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](binTree: BinTree[A]): Int = binTree match {
    case Leaf(_) => 1
    case Branch(l,r) => (depth(r) + 1).max(depth(l) + 1)
  }

  def map[A,B](binTree: BinTree[A])(f: A => B): BinTree[B] = binTree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
}

