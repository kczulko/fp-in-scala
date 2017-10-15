package com.github.kczulko.chapter11

import com.github.kczulko.chapter7.blocking.Par
import com.github.kczulko.chapter7.blocking.Par.Par

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A],F[B]]): F[Either[A,B]] = e match {
    case Right(fb) => map(fb)(Right(_))
    case Left(fa) => map(fa)(Left(_))
  }
}

object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
  }
  implicit val parFunctor: Functor[Par] = new Functor[Par] {
    override def map[A, B](fa: Par[A])(f: (A) => B): Par[B] = Par.map(fa)(f)
  }
}
