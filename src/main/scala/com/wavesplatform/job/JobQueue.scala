package com.wavesplatform.job

trait JobQueue[JobT] {
  def enqueue(job: JobT): Unit
  def shutdownNow(): Unit
}
