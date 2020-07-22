package com.wavesplatform.events.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.ApiRoute
import com.wavesplatform.events.repo.UpdatesRepo

import scala.util.{Failure, Success}

private[http] object routes {
  class GetUpdatesAt(repo: UpdatesRepo) extends ApiRoute {
    import HttpServer._

    override def route: Route = get {
      path("at" / IntNumber) { height =>
        repo.getForHeight(height) match {
          case Success(Some(upd)) => complete(blockchainUpdatedWrites.writes(upd))
          case Success(None)      => complete(StatusCodes.NoContent)
          case Failure(exception) =>
            log.error(s"Failed to get block append for height $height", exception)
            complete(StatusCodes.InternalServerError)
        }
      }
    }
  }

  class GetUpdatesSeq(repo: UpdatesRepo) extends ApiRoute {
    import HttpServer._

    override def route: Route = get {
      path("seq" / IntNumber / IntNumber) { (from, to) =>
        repo.getRange(from, to) match {
          case Success(upds) => complete(upds.map(blockchainUpdatedWrites.writes))
          case Failure(exception) =>
            log.error(s"Failed to get block appends for range [$from, $to]", exception)
            complete(StatusCodes.InternalServerError)
        }
      }
    }
  }
}
