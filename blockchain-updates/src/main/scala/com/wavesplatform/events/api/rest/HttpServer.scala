package com.wavesplatform.events.api.rest

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import com.wavesplatform.events._
import com.wavesplatform.events.repo.UpdatesRepo
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

class HttpServer(port: Int, repo: UpdatesRepo.Read)(implicit actorSystem: ActorSystem) extends ScorexLogging {
  private[this] val combinedRoute = new routes.GetUpdatesAt(repo).route ~
    new routes.GetUpdatesSeq(repo).route ~
    complete(StatusCodes.NotFound)

  private[this] var binding: Http.ServerBinding = _

  def start(): Unit = {
    val httpFuture = Http().bindAndHandle(combinedRoute, "0.0.0.0", port)
    binding = Await.result(httpFuture, 20.seconds)
  }

  def shutdown(): Unit = {
    Try(Await.ready(binding.unbind(), 2.minutes)).failed.map(e => log.error("BlockchainUpdates extension failed to unbind HTTP API", e))
  }

}

object HttpServer {
  implicit val blockchainUpdatedWrites: Writes[BlockchainUpdated] = (a: BlockchainUpdated) =>
    Json.obj(
      "toHeight" -> a.toHeight,
      "toId"     -> a.toId.toString,
      "raw"      -> a.toString
    )
}
