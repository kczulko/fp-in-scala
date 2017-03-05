package com.github.kczulko.chapter15

import java.io.{BufferedWriter, File, FileWriter}

import com.github.kczulko.chapter13.tailrec.TailRec
import com.github.kczulko.chapter15.Process.{filter, lift}

import scala.annotation.tailrec

object Main {

  def toCelsius(fahrenheit: Double): Double =
    (5.0/9.0) * (fahrenheit - 32.0)

  implicit def stringToFile(s: String) = new File(s)

  def processFile2[A](input: java.io.File, output: java.io.File, p: Process[String, Double]): TailRec[Unit] = TailRec {
    @tailrec
    def go(cur: Process[String,Double], ss: Iterator[String], o: BufferedWriter): Unit =
      cur match {
        case Halt() => ()
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
          go(next, ss, o)
        case Emit(h,t) =>
          o.write(h.toString)
          o.write("\n")
          go(t, ss, o)
      }

    val i = io.Source.fromFile(input)
    val o = new BufferedWriter(new FileWriter(output))
    try {
      go(p,i.getLines, o)
    } finally {
      o.close
      i.close
    }
  }

  def pureMain(inputFile: java.io.File, outputFile: java.io.File): TailRec[Unit] = {
    val stringPredicate: String => Boolean = s => !s.isEmpty && !s.startsWith("#")
    val process: Process[String, Double] =
      filter(stringPredicate).map(_.toDouble) |> lift(toCelsius)
    processFile2(inputFile, outputFile, process)
  }

  def main(args: Array[String]): Unit = {
    TailRec.run {
      assert(args.length == 2)
      pureMain(args(0), args(1))
    }
  }
}
