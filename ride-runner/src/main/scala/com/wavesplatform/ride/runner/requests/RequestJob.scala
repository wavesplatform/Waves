package com.wavesplatform.ride.runner.requests

import com.wavesplatform.ride.runner.stats.RideRunnerStats
import kamon.metric.Timer
import monix.execution.CancelablePromise

// Promise to get rid of duplicated requests
class RequestJob private (workers: Int, val timer: Timer.Started, val result: CancelablePromise[RideScriptRunResult]) {
  def inProgress: Boolean   = workers > 0
  def isAvailable: Boolean  = workers <= 1
  def proceed(): RequestJob = new RequestJob(workers + 1, timer, result)
}

object RequestJob {
  def apply(): RequestJob =
    new RequestJob(workers = 0, timer = RideRunnerStats.rideRequestTimeInQueue.start(), CancelablePromise[RideScriptRunResult]())
}
