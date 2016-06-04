package com.kczulko.chapter4


sealed trait MyEither[+E,+A] {
  def map[B](f: A => B): MyEither[E, B]
  def flatMap[EE >: E, B](f: A => MyEither[EE,B]): MyEither[EE,B]
  def orElse[EE >: E, B >: A](b: MyEither[EE,B]): MyEither[EE,B]
  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A,B) => C): MyEither[EE, C]
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing] {
  override def map[B](f: (Nothing) => B): MyEither[E, B] = this

  override def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (Nothing, B) => C): MyEither[EE, C] = this

  override def flatMap[EE >: E, B](f: (Nothing) => MyEither[EE, B]): MyEither[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: MyEither[EE, B]): MyEither[EE, B] = b
}

case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
  override def map[B](f: A => B): MyEither[Nothing, B] = MyRight(f(value))

  override def map2[EE >: Nothing, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    b match {
      case r: MyRight[B] => MyRight(f(value, r.value))
      case l: MyLeft[EE] => l
    }

  override def flatMap[EE >: Nothing, B](f: (A) => MyEither[EE, B]): MyEither[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: MyEither[EE, B]): MyEither[EE, B] = this
}

object MyEither {
  def sequence[E, A](es: List[MyEither[E,A]]): MyEither[E, List[A]] =
    traverse(es)(x => x)

  // initial version of the sequence:
  /*es.foldLeft {
      MyRight(List(): List[A]): MyEither[E, List[A]]
    } {
      (x,y) => y match {
        case r: MyRight[A] => x.flatMap(la => MyRight(la ++ List(r.value)))
        case l: MyLeft[E] => x match {
          case le: MyLeft[E] => le
          case _ => l
        }
      }
    }*/

  def traverse[E,A,B](as: List[A])(f: A => MyEither[E,B]): MyEither[E, List[B]] =
    as.foldLeft {
      as match {
        case _ => (MyRight(List(): List[B]): MyEither[E, List[B]])
      }
    } {
      (x, y) => x.flatMap(vx => f(y).map(vx ++ List(_)))
    }
}
