package com.github.kczulko.chapter8.myimpl

import com.github.kczulko.chapter6.State
import com.github.kczulko.chapter8.myimpl.Prop.{FailedCase, SuccessCount}
import com.kczulko.chapter6.{RNG, SimpleRNG}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(
      State(
        rng => {
          val (a, nextRng) = sample.run(rng)
          f(a).sample.run(nextRng)
        }
      )
    )

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n,this))
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(other: Prop) = (check, other.check) match {
    case (Right(a), Right(b)) => Right(a + b)
    case (Right(a), Left((f,b))) => Left((f, a + b))
    case (Left((f,a)), Right(b)) => Left((f, a + b))
    case (Left((fa,a)), Left((fb,b))) => Left(s"$fa\n$fb", a + b)
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase =  String
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    assert(stopExclusive > start, "required: stopExclusive > start")
    Gen[Int](State[RNG,Int](rng => {
      val (value, nextRng) = rng.nextInt
      val range = stopExclusive - start
      val result = value % range + start
      (result, nextRng)
    }))
  }

  def unit[A](a: => A): Gen[A] = Gen[A](State.unit(a))

  def boolean: Gen[Boolean] = {
    SimpleRNG
    val binaryGen = choose(0,2)
    Gen[Boolean](State[RNG,Boolean](rng => {
      val (i,nextRng) = binaryGen.sample.run(rng)
      val result = if (i == 0) false else true
      (result, nextRng)
    }))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](gen: Gen[A]): Gen[List[A]] = ???

  def forAll[A](gen: Gen[A])(p: A => Boolean): Prop = ???

  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] = Gen[A](State[RNG,A](rng => {
    val (choose,_) = boolean.sample.run(rng)
    if (choose) {
      gen1.sample.run(rng)
    } else {
      gen2.sample.run(rng)
    }
  }))

  def union2[A](gen1: Gen[A], gen2: Gen[A]) = boolean.flatMap(if(_) gen1 else gen2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1,w1) = g1
    val (gen2,w2) = g2

    assert(w1 + w2 == 1.0)

    Gen[Double](SimpleRNG(0).double).flatMap(v => if (v % (w1+w1) <= w1) gen1 else gen2)
  }
}

