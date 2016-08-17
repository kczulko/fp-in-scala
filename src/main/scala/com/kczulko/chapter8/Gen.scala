package com.kczulko.chapter8

import com.kczulko.chapter5.MyStream
import com.kczulko.chapter6.{SimpleRNG, State, RNG}
import com.kczulko.chapter8.Prop.{TestCases, FailedCase, SuccessCount}

sealed trait Result {
  def isFalsified = false
}

case object Passed extends Result {
  override def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}

/*trait Prop {
  def &&(p: Prop): Prop = ???
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
}*/

case class Prop(run: (TestCases,RNG) => Result)

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN_1(size: Int): Gen[List[A]] = flatMap(a => Gen(State(s => (List.fill(size)(a), s))))
}

object Gen {
  def listOf[B](a: Gen[B]): Gen[List[B]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(a)(rng).zipWith(MyStream.from(0))((a,b) => (a,b)).take(n).map {
        case (a,i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.toList.find(_.isFalsified).getOrElse(Passed)
  }

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case $s\n" +
    s"generated an exception: ${e.getMessage()}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  private def randomStream[A](g: Gen[A])(rng: RNG): MyStream[A] = {
    MyStream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))

  def boolean: Gen[Boolean] = Gen(State(rng => {
    val (i, r) = rng.nextInt
    (i < 0, r)
  }))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State((rng: RNG) => rng.nonNegativeInt(rng)).map(n => start + n % (stopExclusive-start)))

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = Gen(
    a.sample.map(aElem => List.fill(n)(aElem))
  )

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if(b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(SimpleRNG.apply(0).double).flatMap(d => if (d < g1Threshold) g1._1 else g2._1)
  }
}