package com.wavesplatform.events.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.ApiRoute
import com.wavesplatform.events._
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

class HttpServer(port: Int, repo: UpdatesRepo)(implicit actorSystem: ActorSystem) extends ScorexLogging {
  private[this] val combinedRoute = new GetUpdatesAtRoute(repo).route ~ complete(StatusCodes.NotFound)

  private[this] var binding: Http.ServerBinding = _

  def start(): Unit = {
    val httpFuture = Http().bindAndHandle(combinedRoute, "0.0.0.0", port)
    binding = Await.result(httpFuture, 20.seconds)
  }

  def shutdown(): Unit = {
    Try(Await.ready(binding.unbind(), 2.minutes)).failed.map(e => log.error("BlockchainUpdates extension failed to unbind HTTP API", e))
  }

}

private[this] class GetUpdatesAtRoute(repo: UpdatesRepo) extends ApiRoute {
  override def route: Route = get {
    path("at" / IntNumber) { height =>
      repo.getForHeight(height) match {
        case Some(upd) => complete(Json.obj("update" -> upd.toString))
        case None      => complete(StatusCodes.NoContent)
      }
    }
  }
}
