package com.wavesplatform.ride.runner.db

import monix.execution.{Cancelable, Scheduler}

trait RideDb extends AutoCloseable {
  def access: RideDbAccess
  def startCollectingStats(scheduler: Scheduler): Cancelable = Cancelable.empty
}
