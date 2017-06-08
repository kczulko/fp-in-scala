package com.github.kczulko.chapter7

import java.util.concurrent.{ExecutorService, Executors}

object ExecutionContext {
  def executionContext(poolSize: Int)(test: ExecutorService => Any): Unit = {
    val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)
    try {
      test(pool)
    } finally {
      pool.shutdown()
    }
  }
}
