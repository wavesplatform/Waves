package com.wavesplatform.ride

import akka.Done
import akka.actor.CoordinatedShutdown

import scala.concurrent.Future

package object app {
  final implicit class CoordinatedShutdownOps(private val self: CoordinatedShutdown) extends AnyVal {
    def cleanup(afterPhase: CustomShutdownPhase)(f: => Unit): Unit = cleanupTask(afterPhase, "general")(f)

    def cleanupTask(afterPhase: CustomShutdownPhase, taskName: String)(f: => Unit): Unit =
      self.addTask(afterPhase.name, taskName) { () =>
        f
        Future.successful(Done)
      }
  }
}
