package com.github.kczulko.chapter8.myimpl

import com.github.kczulko.chapter5.MyStream
import com.github.kczulko.chapter6.State
import com.github.kczulko.chapter8.myimpl.Prop._
import com.kczulko.chapter6.{RNG, SimpleRNG}

import scala.util.{Failure, Success, Try}

case class Gen[+A](sample: State[RNG,A]) {
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

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  override def isFalsified: Boolean = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(other: Prop): Prop = Prop {
    (ms,tc,rng) =>
      run(ms,tc,rng) match {
        case Passed => other.run(ms,tc,rng)
        case any => any
      }
  }

  def ||(p: Prop): Prop = Prop {
    (m,n,rng) => this.run(m,n,rng) match {
      case Passed => Passed
      case _ => p.run(m,n,rng)
    }
  }
}

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase =  String
  type MaxSize = Int

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG.apply(System.currentTimeMillis)): Unit =
    p.run(maxSize,testCases,rng) match {
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Falsified(failedCase, successCount) => println(s"! Falsified after $successCount passed tests:\n $failedCase")
    }
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

  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] = Gen[A](State[RNG,A](rng => {
    val (choose,_) = boolean.sample.run(rng)
    if (choose) {
      gen1.sample.run(rng)
    } else {
      gen2.sample.run(rng)
    }
  }))

  def union2[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] = boolean.flatMap(if(_) gen1 else gen2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1,w1) = g1
    val (gen2,w2) = g2

    assert(w1 + w2 == 1.0)

    Gen[Double](SimpleRNG(0).double).flatMap(v => if (v % (w1+w1) <= w1) gen1 else gen2)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n,rng) =>
      val generatedStream = MyStream.unfold(rng)(randGen => Some(as.sample.run(randGen)))

      generatedStream.zipWith(MyStream.from(0))((_,_)).take(n).map {
        case (a,i) => Try(f(a)) match {
          case Success(res) if res => Passed
          case Failure(ex) => Falsified(ex.getMessage, i)
          case _ => Falsified(a.toString, i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }
}

case class SGen[+A](forsize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(s =>
      forsize(s).flatMap(a => f(a).forsize(s))
    )
}

case object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    forsize => Gen.listOfN(forsize, g)
  }

  def nonEmptyListOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    forsize => Gen.listOfN(forsize max 1, g)
  }

  def forAll[A](sGen: SGen[A])(f: A => Boolean): Prop = forAll(sGen.forsize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => Gen.forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop{
        (max,_,rng) => p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)

      prop.run(max,n,rng)
    }
  }
}

