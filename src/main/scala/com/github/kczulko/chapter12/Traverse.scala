package com.github.kczulko.chapter12

import com.github.kczulko.chapter10.{Foldable, Monoid}
import com.github.kczulko.chapter11.{Functor, Monads}
import com.github.kczulko.chapter12.Applicative.Const
import com.github.kczulko.chapter12.Applicatives.optionApp
import com.github.kczulko.chapter6.State
import com.github.kczulko.chapter6.State.{get, set}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] = {
    traverse(fa)(a => optionApp.unit(f(a))) match {
      case Some(fb) => fb
      case None => throw new IllegalStateException("inconceivable!")
    }
  }

  def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f, A, Nothing](as)(f)(Applicative.monoidApplicative(m))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monads.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => (for {
    i <- get[Int]
    _ <- set(i+1)
  } yield (a, i))).run(0)._1

  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ())).run(Nil)._2.reverse

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A,S) => (B,S)): (F[B], S) =
    traverseS(fa)((a:A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b)).run(s)

  def toList2[A](fa: F[A]): List[A] = mapAccum(fa, List[A]())((a,s) => ((), a :: s))._2.reverse
  def zipWithIndex2[A](fa: F[A]): F[(A, Int)] = mapAccum(fa, 0)((a,s) => ((a,s), s+1))._1

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_,as) => (as.head, as.tail))._1

  def fuse[G[_] : Applicative, H[_] : Applicative, A, B](fa: F[A])(f: A => G[B], g: A => H[B]): (G[F[B]], H[F[B]]) =
    (traverse(fa)(f), traverse(fa)(g))

  def compose[G[_]](implicit g: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
    override def foldLeft[A, B](as: F[G[A]])(z: B)(f: (B, A) => B): B = ???
    override def foldMap[A, B](as: F[G[A]])(f: (A) => B)(m: Monoid[B]): B = ???
    override def foldRight[A, B](as: F[G[A]])(z: B)(f: (A, B) => B): B = ???
    override def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] = ???
    override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) = Traverse.this.traverse(fa)((ga: G[A]) => g.traverse(ga)(f))
  }
}
