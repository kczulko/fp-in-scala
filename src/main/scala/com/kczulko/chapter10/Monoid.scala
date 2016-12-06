package com.kczulko.chapter10

trait Monoid[A] {
  // satisfies associativity law
  def op(a1: A, a2: A): A
  // satisfies left and right op invariant law:
  // op(zero, A) = A && op(A, zero) = A
  def zero: A
}

object Monoids {
  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    override def zero: (A) => A = x => x
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b,a) => m.op(b,f(a)))

  def foldRight[A,B](as: List[A], zero: B)(f: (A,B) => B): B =
    foldMap(as, endoMonoid: Monoid[B => B])(f.curried)(zero)

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 => m.zero
    case 1 => f(v.head)
    case _ => {
      val splitted = v.splitAt(v.length/2)
      m.op(foldMapV(splitted._1, m)(f), foldMapV(splitted._2, m)(f))
    }
  }

//  def isOrdered[A : Ordering](v: IndexedSeq[A]): Boolean = {
//  }

  def wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l,w,r)) => Part(a+l, w, r)
      case (Part(l,w,r), Stub(a)) => Part(l, w, r + a)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
    }
    override def zero: WC = Stub("")
  }

  def countWords(s: String): Int = {
    def wc(c: Char): WC = c.isWhitespace match {
      case true => Part("", 0, "")
      case _ => Stub(c.toString)
    }
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(b) => unstub(b)
      case Part(l,w,r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))
    override def zero: (A, B) = (ma.zero, mb.zero)
  }

}