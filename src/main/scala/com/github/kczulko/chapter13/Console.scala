package com.github.kczulko.chapter13

import com.github.kczulko.chapter13.Translate.~>
import com.github.kczulko.chapter13.free.{Free, Suspend}
import com.github.kczulko.chapter7.blocking.Par
import com.github.kczulko.chapter7.blocking.Par.Par

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
  def toReader: ConsoleReader[A]
}

case object ReadLine extends Console[Option[String]] {
  override def toPar: Par[Option[String]] = Par.lazyUnit(run)

  override def toThunk: () => Option[String] = () => run

  def run: Option[String] = try Some(scala.io.StdIn.readLine())
  catch { case _: Exception => None }

  override def toReader: ConsoleReader[Option[String]] = ConsoleReader(Some(_))
}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(line))
  override def toThunk: () => Unit = () => println(line)
  override def toReader: ConsoleReader[Unit] = ConsoleReader(_ => ())
}

object Console {
  type ConsoleIO[A] = Free[Console,A]
  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }
  val consoleToConsoleReader = new (Console ~> ConsoleReader) {
    override def apply[A](f: Console[A]): ConsoleReader[A] = f.toReader
  }
}
