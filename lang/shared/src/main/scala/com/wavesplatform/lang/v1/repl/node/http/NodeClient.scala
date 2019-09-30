package com.wavesplatform.lang.v1.repl.node.http

import cats.Functor
import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.repl.global
import com.wavesplatform.lang.v1.repl.node.http.NodeClient.ResponseWrapper
import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse
import io.circe.Decoder
import io.circe.parser.decode

import scala.concurrent.ExecutionContext.Implicits.{global => g}
import scala.concurrent.Future
import scala.language.higherKinds

private[node] case class NodeClient(baseUrl: String) {
  def get[F[_] : Functor : ResponseWrapper, R: Decoder](path: String): Future[F[R]] =
    global.requestNode(baseUrl + path)
      .map(r => r: F[String])
      .map(_.map(decode[R]).map(_.explicitGet()))
}

object NodeClient {
  type ResponseWrapper[F[_]] = NodeResponse => F[String]

  implicit val optionResponse: NodeResponse => Option[String] = {
    case NodeResponse(404, _)    => None
    case NodeResponse(200, body) => Some(body)
    case NodeResponse(_,   body) => throw new RuntimeException(body)
  }

  implicit val eitherResponse: NodeResponse => Either[String, String] = {
    case NodeResponse(200, body) => Right(body)
    case NodeResponse(_,   body) => Left(body)
  }

  implicit val idResponse: NodeResponse => String = {
    case NodeResponse(200, body) => body
    case NodeResponse(_,   body) => throw new RuntimeException(body)
  }
}
