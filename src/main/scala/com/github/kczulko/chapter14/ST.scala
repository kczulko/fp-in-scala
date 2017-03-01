package com.github.kczulko.chapter14

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    override protected def run(s: S): (B, S) = {
      val (a,s1) = self.run(s)
      (f(a),s)
    }
  }

  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1): (A, S) = self.run(s)
      f(a).run(s)
    }
  }

}
object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      override protected def run(s: S): (A, S) = (memo, s)
    }
  }
}
sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S,Int] = ST(value.size)

  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    override protected def run(s: S): (Unit, S) = {
      value(i) = a
      ((),s)
    }
  }

  def swap(i: Int, j: Int): ST[S,Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()

  def read(i: Int): ST[S,A] = ST(value(i))

  def freeze: ST[S,List[A]] = ST(value.toList)

  def fill(xs: Map[Int,A]): ST[S,Unit] = {
    xs.foldRight(ST[S,Unit](()))((p,b) => {
      write(p._1,p._2)
    })
  }
}
object STArray {
  def apply[S,A:Manifest](sz: Int, v: A): STArray[S,A] = new STArray[S,A] {
    override protected def value: Array[A] = Array.fill(sz)(v)
  }
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    override protected def run(s: S): (Unit, S) = {
      cell = a
      ((),s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    override protected var cell: A = a
  })
}