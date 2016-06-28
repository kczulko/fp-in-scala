package com.kczulko.chapter6

import scala.math.abs

sealed trait RNG {
  type Rand[A] = State[RNG,A]
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG)
  def double : Rand[Double]
  def ints(count: Int): List[Int]
  def intDouble: Rand[(Int,Double)]
  def doubleInt: Rand[(Double,Int)]
  def double3(rng:RNG): ((Double, Double, Double), RNG)

  val int: Rand[Int] = State(_.nextInt)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def unit[A](a: A): Rand[A] = State(rng => (a, rng))

  def sequence[A](ls: List[Rand[A]]): Rand[List[A]] =
    State {
      rng => ls.foldLeft((List[A](), rng))((b, ra) => {
        val rb = b._2
        val list = b._1
        val (newB, newRb) = ra.run(rb)
        (list ::: List(newB), newRb)
      })
    }

  def sequence2[A](ls: List[Rand[A]]): Rand[List[A]] = ls.foldRight(unit(List[A]()))((elem, acc) => map2(elem, acc)(_ :: _))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B]) (f: (A, B) => C) : Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    State{
      rng => {
        val (a, r) = f.run(rng)
        g(a).run(r)
      }
    }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }

  override def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => (Int.MaxValue, r)
    case (v, r) if v < 0 => (abs(v), r)
    case (v, r) => (v, r)
  }

  override def ints(count: Int): List[Int] = {
    { 1 to (count - 1) }.foldLeft(List(nextInt))((b, i) =>
      List(b.head._2.nextInt) ++ b
    ).map(v => v._1)
  }

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  override def intDouble: Rand[(Int, Double)] = both(int, double)

  override def doubleInt: Rand[(Double, Int)] = both(double, int)

  override def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val first = double.run(rng)
    val second = double.run(first._2)
    val third = double.run(second._2)
    ((first._1, second._1, third._1), third._2)
  }

  override def double: Rand[Double] = map(State(nonNegativeInt))({
    case Int.MaxValue => Int.MaxValue - 1 / Int.MaxValue
    case v => v / Int.MaxValue
  })

  private def both[A,B](r1: Rand[A], r2: Rand[B]): Rand[(A,B)] = map2(r1, r2)((_,_))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(State(nonNegativeInt))(a => {
      val mod = a % n
      if (a + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    })
}
