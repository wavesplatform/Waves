package com.wavesplatform.ride.runner.entrypoints

import akka.actor.CoordinatedShutdown.Reason

object ProgramFinishedReason extends Reason {
  override def toString: String = "ProgramFinishedReason"
}
