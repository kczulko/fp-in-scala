package com.github.kczulko.chapter5

trait MyStream[+A] {
  import MyStream._

  def toList: List[A]

  def drop(n: Int): MyStream[A]

  def take(n: Int): MyStream[A] = unfold((this, n))(
    _ match {
      case (Cons(h, t), counter) if (counter > 0) => Some(h(), (t(), counter - 1))
      case _ => None
    }
  )

  def zipWith[B, C](other: MyStream[B])(f: (A, B) => C): MyStream[C] =
    unfold((this, other))
      {
        case (_, Empty) | (Empty, _) => None
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      }

  def zipAll[B](other: MyStream[B]): MyStream[(Option[A], Option[B])] =
    unfold((this, other)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(
        (Some(h1()), Some(h2())),
        (t1(), t2())
      )
      case (Cons(h1, t1), Empty) => Some(
        (Some(h1()), None),
        (t1(), Empty)
      )
      case (Empty, Cons(h2, t2)) => Some(
        (None, Some(h2())),
        (Empty, t2())
      )
      case _ => None
    }

  def startsWith[B >: A](other: MyStream[B]): Boolean =
    (this, other) match {
      case (Cons(_, _), Cons(_, _)) =>
        unfold((this, other, constant(true))) {
          case (Cons(h1, t1), Cons(h2, t2), Cons(h3, _)) if h3() => Some(
            h1() equals h2(),
            (t1(), t2(), constant(h1() equals h2()))
          )
          case (Cons(h1, t1), Cons(h2, t2), _) => Some(
            false,
            (Empty, Empty, constant(false))
          )
          case _ => None
        }.take(1).forall(_ equals true)
      case _ => false
    }

  def tails: MyStream[MyStream[A]] = MyStream(MyStream()) append unfold(this) {
    case Cons(h, t) => Some(Cons(h,t), t())
    case _ => None
  }

  def tails2: MyStream[MyStream[A]] = this.foldRight(MyStream(MyStream()): MyStream[MyStream[A]])((a, b) =>
    {
      val newElem = (b take(1) append MyStream(MyStream(a))).flatMap(x => x)
      b append MyStream(newElem)
    }
  )

  def scanRight[B](b: B)(f: (A, => B) => B): MyStream[B] =
    MyStream(b) append foldRight(b, MyStream(): MyStream[B])((a,b) => {
      val newFirst = f(a, b._1)
      val newSecond = cons(newFirst, b._2)
      (newFirst, newSecond)
    })._2

  def hasSubsequence[B >: A](other: MyStream[B]) = tails exists(_ startsWith other)

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false;
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  def forall(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def takeWhile(p: (A) => Boolean): MyStream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def takeWhile2(p: (A) => Boolean): MyStream[A]
      = foldRight(Empty: MyStream[A])((a,b) => if (p(a)) cons(a, b) else Empty)

  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): MyStream[B] =
    foldRight(Empty: MyStream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): MyStream[A] = foldRight(Empty: MyStream[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => MyStream[B]): MyStream[B] = s.foldRight(this: MyStream[B])((a, b) => cons(a,b))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = foldRight(Empty: MyStream[B])((a, b) => b append f(a))

  def find(p: A => Boolean): Option[A] = this match {
    case Cons(h,_) if p(h()) => Some(h())
    case Empty => None
    case Cons(_, t) => t().find(p)
  }
}

case object Empty extends MyStream[Nothing] {
  override def toList: List[Nothing] = Nil
  override def drop(n: Int): MyStream[Nothing] = this
}

case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A] {
  override def toList: List[A] = {
    List(h()) ++ t().toList
  }

  override def drop(n: Int): MyStream[A] = n match {
    case 0 => this
    case _ => t().drop(n-1)
  }
}

object MyStream {
  def cons[A](hd: => A, t: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): MyStream[A] = unfold(a)(v => Some(v, v))

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def from(n: Int): MyStream[Int] = unfold(n)(v => Some(v, v+1))

  def fib: MyStream[Int] = unfold((0, 1))(tuple => Some(tuple._1, (tuple._2, tuple._2 + tuple._1)))

  def fib2: MyStream[Int] = {
    def loop(a: Int, b: Int): MyStream[Int] = cons(a, loop(b, a + b))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = {
    def loop(v: S): MyStream[A] = f(v) match {
      case Some((a, s)) => cons(a, loop(s))
      case _ => Empty
    }

    loop(z)
  }
}
