package com.wavesplatform.ride.runner.db

trait RideDb extends AutoCloseable {
  def access: RideDbAccess
  def sendStats(): Unit
}
