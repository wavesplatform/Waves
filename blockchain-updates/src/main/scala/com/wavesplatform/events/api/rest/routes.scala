package com.wavesplatform.events.api.rest

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.ApiRoute
import com.wavesplatform.events.repo.UpdatesRepo

import scala.util.{Failure, Success}

private[rest] object routes {
  class GetUpdatesAt(repo: UpdatesRepo.Read) extends ApiRoute {
    import HttpServer._

    override def route: Route = get {
      path("at" / IntNumber) { height =>
        repo.updateForHeight(height) match {
          case Success(Some(upd)) => complete(blockchainUpdatedWrites.writes(upd))
          case Success(None)      => complete(StatusCodes.NoContent)
          case Failure(exception) =>
            log.error(s"Failed to get block append for height $height", exception)
            complete(StatusCodes.InternalServerError)
        }
      }
    }
  }

  class GetUpdatesSeq(repo: UpdatesRepo.Read) extends ApiRoute {
    import HttpServer._

    override def route: Route = get {
      path("seq" / IntNumber / IntNumber) { (from, to) =>
        repo.updatesRange(from, to) match {
          case Success(upds) => complete(upds.map(blockchainUpdatedWrites.writes))
          case Failure(exception) =>
            log.error(s"Failed to get block appends for range [$from, $to]", exception)
            complete(StatusCodes.InternalServerError)
        }
      }
    }
  }
}
