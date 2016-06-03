package com.kczulko.chapter4

object Playground {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .map(m => xs.map(v => v - m))

    None
  }
}
