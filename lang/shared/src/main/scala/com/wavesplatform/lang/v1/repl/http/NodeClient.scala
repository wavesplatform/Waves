package com.wavesplatform.lang.v1.repl.http

import cats.Functor
import cats.implicits._
import com.softwaremill.sttp.{Response, sttp, _}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.repl.global.sttpBackend
import io.circe.Decoder
import io.circe.parser.decode

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.higherKinds

case class NodeClient(baseUrl: String) {
  type ResponseWrapper[F[_]] = Response[String] => F[String]

  def get[F[_] : Functor : ResponseWrapper, R : Decoder](path: String): F[R] =
    (getRaw(path): F[String])
      .map(decode[R])
      .map(_.explicitGet())

  private val schemaRegex = "^\\w+://.+".r
  private val timeout = 5 seconds

  private def getRaw(path: String): Response[String] = {
    val urlPrefix =
      if (schemaRegex.findFirstMatchIn(baseUrl).nonEmpty) ""
      else "http://"
    val url = urlPrefix + baseUrl + path
    val request = sttp.get(uri"$url").copy(headers = List(("User-Agent", "Chrome")))
    Await.result(request.send(), timeout)
  }
}

object NodeClient {
  implicit val optionResponse: Response[String] => Option[String] = {
    case r if r.code == 404 => None
    case r                  => Some(r.body.explicitGet())
  }

  implicit val eitherResponse: Response[String] => Either[String, String] =
    _.body

  implicit val idResponse: Response[String] => String =
    _.unsafeBody
}
