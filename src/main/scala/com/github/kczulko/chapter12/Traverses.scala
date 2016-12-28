package com.github.kczulko.chapter12

import com.github.kczulko.chapter10.Monoid
import com.github.kczulko.chapter3.datastructures.BinTree

object Traverses {
  def listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] = {
      val ga = implicitly[Applicative[G]]
      fa.foldRight(ga.unit(List[B]()))((a,b) => ga.map2(f(a), b)(_ :: _))
    }
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: (A) => B)(m: Monoid[B]): B = foldRight(as)(m.zero)((a,b) => m.op(f(a),b))
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
    override def map[A, B](fa: List[A])(f: (A) => B) = fa map f
  }

  def optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def sequence[G[_] : Applicative, A](fga: Option[G[A]]): G[Option[A]] = fga match {
      case Some(ga) => implicitly[Applicative[G]].map(ga)(Some(_))
      case _ => implicitly[Applicative[G]].unit(None)
    }
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
    override def foldMap[A, B](as: Option[A])(f: (A) => B)(m: Monoid[B]) =
      as.foldRight(m.zero)((a,b) => m.op(f(a),b))
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
    override def map[A, B](fa: Option[A])(f: (A) => B) = fa map f
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])
  def treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def sequence[G[_] : Applicative, A](fga: Tree[G[A]]): G[Tree[A]] = {
      val ga = implicitly[Applicative[G]]
      fga match {
        case Tree(ag, Nil) => ga.map(ag)(Tree(_, Nil))
        case Tree(ag, t) => ga.map2(ag, listTraverse.traverse(t)(sequence(_)))(Tree(_,_))
      }
    }
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Tree(a, Nil) => f(z,a)
      case Tree(a,t) => f(t.foldLeft(z)((_,tr) => foldLeft(tr)(z)(f)), a)
    }
    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(m: Monoid[B]): B = as match {
      case Tree(a, Nil) => m.op(f(a), m.zero)
      case Tree(a, t) => m.op(f(a), t.foldRight(m.zero)((tr,_) => foldMap(tr)(f)(m)))
    }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Tree(a, Nil) => f(a, z)
      case Tree(a, t) => f(a, t.foldRight(z)((tr,_) => foldRight(tr)(z)(f)))
    }
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] =
      Tree[B](f(fa.head), fa.tail.map(map(_)(f)))
  }
}
