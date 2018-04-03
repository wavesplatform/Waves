package com.wavesplatform.discovery

import java.util.concurrent.TimeUnit

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.TextMessage
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import com.wavesplatform.discovery.actors.MainActor
import com.wavesplatform.discovery.actors.MainActor.WebSocketConnected
import com.wavesplatform.discovery.CancellableExt._
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object DiscoveryApp extends App with ScorexLogging {

  implicit val ec: ExecutionContext                = ExecutionContext.global
  implicit val system: ActorSystem                 = ActorSystem("Default")
  implicit val flowMaterializer: ActorMaterializer = ActorMaterializer()

  import akka.http.scaladsl.server.Directives._

  val (route, timer) = Settings.default.chains
    .map { cs =>
      val mainActor = MainActor(cs.chainId, Settings.default.workersCount)
      mainActor ! MainActor.Peers(cs.initialPeers.toSet)

      val route = get {
        path(cs.chainId.toLower.toString) {

          val sink: Sink[akka.http.scaladsl.model.ws.Message, _] = Sink.ignore
          val source: Source[akka.http.scaladsl.model.ws.Message, NotUsed] =
            Source
              .actorRef[String](1, OverflowStrategy.dropTail)
              .mapMaterializedValue { actor =>
                mainActor ! WebSocketConnected(actor)
                NotUsed
              }
              .map((outMsg: String) => TextMessage(outMsg))

          handleWebSocketMessages(Flow.fromSinkAndSource(sink, source))
        }
      }

      (route, system.scheduler.schedule(FiniteDuration(0, TimeUnit.SECONDS), Settings.default.discoveryInterval, mainActor, MainActor.Discover))
    }
    .reduce((a, b) => (a._1 ~ b._1, a._2.combine(b._2)))

  val binding = Http().bindAndHandle(route, Settings.default.webSocketHost, Settings.default.webSocketPort)

  sys.addShutdownHook {
    binding
      .flatMap(_.unbind())
      .onComplete(_ => {
        timer.cancel()
        system.terminate()
      })
  }

  log.info(s"Server is now online at http://${Settings.default.webSocketHost}:${Settings.default.webSocketPort}")
}
