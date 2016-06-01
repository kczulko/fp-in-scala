package com.kczulko.chapter3.datastructures

import com.kczulko.chapter3.datastructures.BinTree.{depth, map, maximum}
import org.scalatest.{FlatSpec, Matchers}

class BinTreeTests extends FlatSpec with Matchers {
  "size" should "return amount of nodes within the tree" in {
    BinTree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldEqual 5
  }

  "maximum" should "return max value from whole tree" in {
    maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldEqual 3
  }

  "depth" should "return maximum path length from the root of a tree" in {
    depth(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))) shouldEqual 3
  }

  "map" should "change tree elements with given function" in {
    map(Branch(Branch(Leaf(10), Leaf(20)), Leaf(30)))(_ - 1) should be
      (Branch(Branch(Leaf(9), Leaf(19)), Leaf(29)))
  }
}