package com.wavesplatform.ride.runner.entrypoints

import org.apache.pekko.actor.CoordinatedShutdown.Reason

object ProgramFinishedReason extends Reason {
  override def toString: String = "ProgramFinishedReason"
}
