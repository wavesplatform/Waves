package com.wavesplatform.job

trait JobPool[JobT] {
  def add(job: JobT): Unit
  def shutdownNow(): Unit
}
