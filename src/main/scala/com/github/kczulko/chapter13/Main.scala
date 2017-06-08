package com.github.kczulko.chapter13

import com.github.kczulko.chapter11.Monads
import com.github.kczulko.chapter13.Console._
import com.github.kczulko.chapter13.free.Free
import com.github.kczulko.chapter7.blocking.Par.Par

object Main {
  import Monads.function0Monad
  import Monads.parMonad

  def main(args: Array[String]) {
    val f1: ConsoleIO[Option[String]] = for {
      _ <- printLn("I can only interact with the console!")
      ln <- readLn
    } yield ln

    println(f1)
  }

  def runConsoleFunction0[A](a: Free[Console,A]): () => A =
    Free.runFree(a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console,A]): Par[A] =
    Free.runFree(a)(Console.consoleToPar)
}
