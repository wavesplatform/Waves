package com.wavesplatform.ride.runner.requests

import monix.execution.CancelablePromise

// Promise to get rid of duplicated requests
class RequestJob private (workers: Int, val result: CancelablePromise[RideScriptRunResult]) {
  def inProgress: Boolean   = workers > 0
  def isAvailable: Boolean  = workers <= 1
  def proceed(): RequestJob = new RequestJob(workers + 1, result)
}

object RequestJob {
  def apply(): RequestJob = new RequestJob(workers = 0, CancelablePromise[RideScriptRunResult]())
}
