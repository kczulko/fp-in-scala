package com.kczulko.chapter4


trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B]
  def filter(f: A => Boolean): MyOption[A]
  def flatMap[B](f: A => MyOption[B]): MyOption[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](default: => MyOption[B]): MyOption[B]
}

case class MySome[A](value: A) extends MyOption[A] {
  override def map[B](f: (A) => B): MyOption[B] = MySome(f(value))

  override def flatMap[B](f: A => MyOption[B]): MyOption[B] = f(value)

  override def filter(f: A => Boolean): MyOption[A] = f(value) match {
    case true => MySome(value)
    case _ => MyNone
  }

  override def getOrElse[B >: A](default: => B): B = value

  override def orElse[B >: A](default: => MyOption[B]): MyOption[B] = this
}

case object MyNone extends MyOption[Nothing] {
  override def map[B](f: Nothing => B): MyOption[B] = this

  override def flatMap[B](f: (Nothing) => MyOption[B]): MyOption[B] = this

  override def filter(f: (Nothing) => Boolean): MyOption[Nothing] = this

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](default: => MyOption[B]): MyOption[B] = default
}
