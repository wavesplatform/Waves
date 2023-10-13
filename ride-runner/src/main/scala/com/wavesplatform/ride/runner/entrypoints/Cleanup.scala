package com.wavesplatform.ride.runner.entrypoints

import akka.Done
import akka.actor.{ActorSystem, CoordinatedShutdown}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Cleanup(actorSystem: ActorSystem) {
  private val cs = CoordinatedShutdown(actorSystem)

  def cleanup(afterPhase: CustomShutdownPhase)(f: => Unit): Unit = cleanupTask(afterPhase, "general")(f)

  def cleanupTask(afterPhase: CustomShutdownPhase, taskName: String)(f: => Unit): Unit =
    cs.addTask(afterPhase.name, taskName) { () =>
      Future {
        f
        Done
      }(actorSystem.dispatcher)
    }

  def forceStop(): Unit = Await.result(cs.run(ProgramFinishedReason), Duration.Inf)
}
