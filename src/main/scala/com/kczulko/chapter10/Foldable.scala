package com.kczulko.chapter10

import com.kczulko.chapter3.datastructures.{BinTree, Branch, Leaf}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)
}

class ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: (A) => B)(m: Monoid[B]): B =
    as.foldRight(m.zero)((a,b) => m.op(f(a), b))
}

class IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(m: Monoid[B]): B =
    as.foldRight(m.zero)((a,b) => m.op(f(a), b))
}

class StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: (A) => B)(m: Monoid[B]): B =
    as.foldRight(m.zero)((a,b) => m.op(f(a), b))
}

class TreeFoldable extends Foldable[BinTree] {
  override def foldRight[A, B](as: BinTree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(v) => f(v,z)
    case Branch(left,right) => foldRight(left)(foldRight(right)(z)(f))(f)
  }

  override def foldLeft[A, B](as: BinTree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(v) => f(z,v)
    case Branch(left,right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
  }

  override def foldMap[A, B](as: BinTree[A])(f: (A) => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((b,a) => m.op(b, f(a)))
}

class OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case Some(v) => f(v,z)
    case None => z
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = foldRight(as)(z)((a,b) => f(b,a))

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(m: Monoid[B]): B = foldRight(as)(m.zero)((a,b) => m.op(f(a), b))
}