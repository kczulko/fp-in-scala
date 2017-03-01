package com.github.kczulko.chapter13

import com.github.kczulko.chapter11.Monad
import com.github.kczulko.chapter13.Console.{ConsoleIO, consoleToConsoleReader}
import com.github.kczulko.chapter13.free.Free

case class ConsoleReader[A](run: String => A) {
  def map[B](f: A => B): ConsoleReader[B] = ConsoleReader(f compose run)
  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] = ConsoleReader(s => (f compose run)(s).run(s))
}

object ConsoleReader {
  implicit val monad = new Monad[ConsoleReader] {
    override def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_ => a)
    override def flatMap[A, B](ma: ConsoleReader[A])(f: (A) => ConsoleReader[B]): ConsoleReader[B] = ma flatMap f
  }

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    Free.runFree(io)(consoleToConsoleReader)
}
