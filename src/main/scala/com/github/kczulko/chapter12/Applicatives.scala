package com.github.kczulko.chapter12

object Applicatives {
  val optionApp = new Applicative[Option] {// primitive combinators
    override def apply[A, B](fab: Option[(A) => B])(fa: Option[A]): Option[B] =
      for {
        ab <- fab
        a <- fa
      } yield ab(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- fa
        b <- fb
      } yield f(a,b)

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  def validationApp[E]: Applicative[({type F[X] = Validation[E, X]})#F] =
    new Applicative[({type F[X] = Validation[E, X]})#F] {
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) = (fa, fb) match {
        case (Success(a1), Success(a2)) => Success(f(a1, a2))
        case (v @ Failure(_,_), Success(_)) => v
        case (Success(_), v @ Failure(_,_)) => v
        case (Failure(h1,t1), Failure(h2,t2)) => Failure[E](h1, t1 ::: h2 :: t2)
      }

      override def apply[A, B](fab: Validation[E, (A) => B])(fa: Validation[E, A]) = apply2[A,B](fab)(fa)
      override def unit[A](a: => A) = Success(a)
    }

}
