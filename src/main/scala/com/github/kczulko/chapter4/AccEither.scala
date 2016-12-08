package com.kczulko.chapter4

sealed trait AccEither[+E >: List[Nothing], +A] {
  def map[B](f: A => B): AccEither[E, B]
  def flatMap[EE >: E, B](f: A => AccEither[EE, B]): AccEither[EE, B]
  def orElse[EE >: E, B >: A](f: => AccEither[EE, B]): AccEither[EE, B]
  def map2[EE >: E, B, C](b: AccEither[EE,B])(f: (A,B) => C): AccEither[EE, C]
}
case class AccRight[+A](value: A) extends AccEither[List[Nothing], A] {
  override def map[B](f: (A) => B): AccEither[List[Nothing], B] = AccRight(f(value))

  override def map2[EE >: List[Nothing], B, C](b: AccEither[EE, B])(f: (A, B) => C): AccEither[EE, C] =
    b.map(f(this.value, _))

  override def flatMap[EE >: List[Nothing], B](f: (A) => AccEither[EE, B]): AccEither[EE, B] =
    f(this.value)

  override def orElse[EE >: List[Nothing], B >: A](f: => AccEither[EE, B]): AccEither[EE, A] = this
}

case class AccLeft[+E](value: List[E]) extends AccEither[List[E], Nothing] {
  override def map[B](f: (Nothing) => B): AccEither[List[E], B] = this

  override def map2[EE >: List[E], B, C](b: AccEither[EE, B])(f: (Nothing, B) => C): AccEither[EE, C] = b match {
    case al: AccLeft[EE] => AccLeft(al.value ++ this.value)
    case _ => this
  }

  override def flatMap[EE >: List[E], B](f: (Nothing) => AccEither[EE, B]): AccEither[EE, B] = this

  override def orElse[EE >: List[E], B >: Nothing](f: => AccEither[EE, B]): AccEither[EE, B] = f match {
    case al: AccLeft[EE] => AccLeft(al.value ++ this.value)
    case ar => ar
  }
}
