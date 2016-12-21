package com.github.kczulko.chapter11

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((fa,b) => map2(fa,b)(_ :: _))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((a,flb) => map2(f(a), flb)(_ :: _))
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = flatMap(ma)(a => unit(List.fill(n)(a)))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case head :: tail => flatMap(f(head))(b => {
      if (!b) filterM(tail)(f) else map(filterM(tail)(f))(head :: _)
    })
  }
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def flatMap2[A,B](ma: F[A])(f: A => F[B]): F[B] = compose(identity[F[A]], f)(ma)
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  def flatMap3[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))
  def compose2[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
}


