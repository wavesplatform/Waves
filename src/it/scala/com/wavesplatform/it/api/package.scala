package com.wavesplatform.it

import org.asynchttpclient.Response
import play.api.libs.json.Format
import play.api.libs.json.Json.parse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

package object api {
  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext): Future[A] = f.map(r => parse(r.getResponseBody).as[A])(ec)
  }

  def waitFor[A](nodes: Iterable[NodeApi])
                (request: NodeApi => Future[A],
                 cond: Iterable[A] => Boolean,
                 retryInterval: FiniteDuration): Future[Boolean] = {
    def retry = Future(Thread.sleep(retryInterval.toMillis)).flatMap(_ => waitFor(nodes)(request, cond, retryInterval))

    Future.traverse(nodes)(request)
      .map(cond)
      .recover { case _ => false }
      .flatMap {
        case true => Future.successful(true)
        case false => retry
      }
  }

  def waitForSameBlocksAt(nodes: Iterable[NodeApi])(height: Int, retryInterval: FiniteDuration): Future[Boolean] = {
    waitFor[NodeApi.Block](nodes)(_.blockAt(height), { blocks => blocks.forall(_ == blocks.head) }, retryInterval)
  }

}
