package com.wavesplatform.actor

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.actor.{ActorSystem, AllForOneStrategy, SupervisorStrategy, SupervisorStrategyConfigurator}
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import scorex.utils.ScorexLogging

object RootActorSystem extends ScorexLogging {
  @volatile private var failed = false

  final class EscalatingStrategy extends SupervisorStrategyConfigurator {
    override def create(): SupervisorStrategy = AllForOneStrategy(loggingEnabled = false) {
      case t: Throwable =>
        failed = true
        log.error("Root actor got exception, escalate", t)
        SupervisorStrategy.Escalate
    }
  }

  def start(id: String)(init: ActorSystem => Unit): Unit = {
    val system = ActorSystem(id, ConfigFactory.load().withValue("akka.actor.guardian-supervisor-strategy",
      ConfigValueFactory.fromAnyRef("com.wavesplatform.actor.RootActorSystem$EscalatingStrategy")))

    sys.addShutdownHook {
      Await.result(system.terminate(), Duration.Inf)
    }

    try {
      init(system)
    } catch {
      case e: Exception =>
        log.error(s"Error while initializing actor system $id", e)
        sys.exit(1)
    }

    Await.result(system.whenTerminated, Duration.Inf)
    if (failed) {
      sys.exit(1)
    } else {
      sys.exit(0)
    }
  }
}
