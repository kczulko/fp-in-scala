package com.github.kczulko.chapter8.myimpl

import java.util.concurrent.Executors

import com.github.kczulko.chapter12.Traverses.Tree
import com.github.kczulko.chapter3.datastructures.BinTree
import com.github.kczulko.chapter7.blocking.Par
import com.kczulko.chapter6.{RNG, SimpleRNG}
import org.scalatest.{FlatSpec, Matchers}

class SGenTest extends FlatSpec with Matchers {

  it should "return success while validating List.max function" in {

    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(SGen.nonEmptyListOf(smallInt)) {
      list =>
        val max = list.max
        !list.exists(_ > max)
    }

    maxProp.run(100, 100, SimpleRNG(System.currentTimeMillis)) shouldEqual Passed
  }

  it should "return success while checking List.sorted property" in {

    def propOf(f: List[Int] => Boolean) = {
      val ints = Gen.choose(-100, 100)
      Prop.forAll(SGen.nonEmptyListOf(ints))(f)
    }

    val lastIsMax = propOf {
      list =>
        val sorted = list.sorted
        sorted.last == list.max
    }

    val firstIsMin = propOf {
      list =>
        val sorted = list.sorted
        sorted.head == list.min
    }

    val nextIsBiggerThanPrevious = propOf { list =>
      list.sorted match {
        case sorted @_ :: next :: tail =>
          (next :: tail).zip(sorted.init).forall({ case (bigger, lesser) => bigger >= lesser })
        case _ =>
          true
      }
    }

    (lastIsMax && firstIsMin && nextIsBiggerThanPrevious)
      .run(
        100,
        100,
        SimpleRNG(System.currentTimeMillis)
      ) shouldEqual Passed
  }

  "it" should "test Par.fork property" in {
    val gen = Gen.choose(1,100).map(Par.unit)
    val es = Executors.newFixedThreadPool(8)

    val prop = Prop.forAllPar(gen){
      parA =>
        Par.map2(parA, Par.fork(parA)) {
          _ equals _
        }
    }

    try {
      prop.run(
        100,
        100,
        SimpleRNG(System.currentTimeMillis)
      ) shouldEqual Passed
    } finally {
      es.shutdown()
    }
  }

  it should "allow to define & test List.take property" in {
    val listGen = SGen.nonEmptyListOf(Gen.choose(0, 100))
    val sizeGen = listGen.flatMap(l =>
      SGen(_ => Gen.choose(0, l.size))
    )

    val prop = Prop.forAll(listGen ** sizeGen) {
      case (list, n) =>
        list.take(n).size == n
    }

    prop.run(100, 100, SimpleRNG(System.currentTimeMillis())) shouldEqual Passed
  }

//  it should "test behavior of Tree.fold function" in {
//    val sgen = SGen(forsize =>
//      Gen.choose(0, 100).map(depth => BinTree.unfold(depth, 1))
//    )
//
//    val prop = Prop.forAll(sgen) {
//      (binTree: BinTree[Int]) =>
//        //BinTree.fold(binTree, (a: Int) => a)((l, r) => l() + r()) == Math.pow(2, BinTree.depth(binTree))
//        true
//    }
//
//    prop.run(100, 100, SimpleRNG(System.currentTimeMillis))
//  }
}
