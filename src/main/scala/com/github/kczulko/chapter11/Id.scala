package com.github.kczulko.chapter11

case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  def map[B](f: A => B): Id[B] = Id(f(value))
}

object IdMonad extends Monad[Id] {
  override def unit[A](a: => A): Id[A] = Id(a)
  override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma flatMap f
}
