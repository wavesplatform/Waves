package com.wavesplatform.lang

import com.softwaremill.sttp.{HttpURLConnectionBackend, MonadError, Request, Response, SttpBackend}
import com.softwaremill.sttp.{Response, sttp, _}
import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class SttpClient {
  private implicit val sttpBackend: SttpBackend[Future, Nothing] = new SttpBackend[Future, Nothing] {
    val internal = HttpURLConnectionBackend()

    override def send[T](request: Request[T, Nothing]): Future[Response[T]] =
      Future(internal.send(request))(ExecutionContext.global)

    override def close(): Unit =
      internal.close()

    override def responseMonad: MonadError[Future] = ???
  }

  private val schemaRegex = "^\\w+://.+".r

  def requestNode(url: String): Future[NodeResponse] = {
    val urlPrefix =
      if (schemaRegex.findFirstMatchIn(url).nonEmpty) ""
      else "http://"

    sttp.get(uri"${urlPrefix + url}")
      .send()
      .map(r => NodeResponse(r.code, r.body.fold(identity, identity)))
  }
}
