package com.wavesplatform.ride.app

import play.api.libs.json.{Json, OFormat}

case class ServiceStatus(
    maxObservedHeight: Int = 0,
    lastProcessedHeight: Int = 0,
    lastProcessedTime: Long = 0,
    nowTime: Long = 0,
    idleMs: Long = 0,
    healthy: Boolean = false
)

object ServiceStatus {
  implicit val serviceStatusFormat: OFormat[ServiceStatus] = Json.format[ServiceStatus]
}
