package com.kczulko.chapter4

object Playground {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs)
      .flatMap(mv => mean(xs.map(v => math.pow(v - mv, 2))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(t => t)

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight {
      a match {
        case Nil => None
        case _ => Some(List()): Option[List[B]]
      }
    }
    {
      (x: A, y:Option[List[B]]) => (f(x), y) match {
        case (Some(p), Some(q)) => Some(List(p) ++ q)
        case (_, _) => None
      }
    }

  // alternative way of traverse impl:
  /*a match {
    case head :: Nil if (f(head).isDefined) => f(head).map(List(_))
    case head :: tail if (f(head).isDefined) => traverse(tail)(f) map (List(f(head).get) ++ _)
    case _ => None
  }*/

  def MyTry[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None }

  def insuranceRateQuote(age: Int, numOfTickets: Int): Double = 4.5

  def map2[A,B,C](first: Option[A], second: Option[B])(f: (A,B) => C): Option[C]
    = first.flatMap(a => second.map(b => f(a,b)))

  def parseInsuranceRateQuote(age: String, numOfTickets: String): Option[Double] = {
    val ageOpt: Option[Int] = MyTry(age.toInt)
    val ticketsOpt = MyTry(numOfTickets.toInt)
    map2(ageOpt, ticketsOpt)(insuranceRateQuote)
  }

  def main(args: Array[String]) {
    def myComposition = lift{math.sqrt} compose { Some(_: Double) }
    variance(Seq(1,2)).get
  }
}
