package com.kczulko.chapter6

case class State[S, +A](run: (S) => (A,S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State{
      s => {
        val (a, r) = run(s)
        g(a).run(r)
      }
    }
}

object State {
  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def map2[A,B,C,S](s1: State[S,A], s2: State[S,B]) (f: (A, B) => C) : State[S,C] = s1.flatMap(a => s2.flatMap(b => unit(f(a,b))))

  def sequence2[S,A](ls: List[State[S,A]]): State[S, List[A]]
    = ls.foldRight(unit[S, List[A]](List()))((elem, acc) => map2(elem, acc)(_ :: _))

  def sequence[S,A](ls: List[State[S,A]]): State[S,List[A]] =
    State {
      rng => ls.foldLeft((List[A](), rng))((b, ra) => {
        val rb = b._2
        val list = b._1
        val (newB, newRb) = ra.run(rb)
        (list ::: List(newB), newRb)
      })
    }
}