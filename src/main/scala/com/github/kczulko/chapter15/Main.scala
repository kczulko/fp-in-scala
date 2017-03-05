package com.github.kczulko.chapter15

import java.io.{BufferedWriter, File, FileWriter}

import com.github.kczulko.chapter13.tailrec.TailRec
import com.github.kczulko.chapter15.Process.{filter, lift}

import scala.annotation.tailrec

object Main {

  def toCelsius(fahrenheit: Double): Double =
    (5.0/9.0) * (fahrenheit - 32.0)

  implicit def stringToFile(s: String) = new File(s)

  def convert(input: java.io.File, output: java.io.File): TailRec[Unit] = TailRec {
    @tailrec
    def go(cur: Process[String,_], ss: Iterator[String]): Unit =
      cur match {
        case Halt() => ()
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
          go(next, ss)
        case Emit(_,t) =>
          go(t, ss)
      }

    val in = io.Source.fromFile(input)
    val out = new BufferedWriter(new FileWriter(output))
    try {
      go(
        filter[String](s => !s.isEmpty && !s.trim.startsWith("#")).map(_.toDouble) |>
          lift(toCelsius) |>
          lift[Double, Unit](d => out.write(d.toString + "\n")),
        in.getLines
      )
    } finally {
      out.close()
      in.close
    }
  }

  def main(args: Array[String]): Unit = {
    TailRec.run {
      assert(args.length == 2)
      convert(args(0), args(1))
    }
  }
}
