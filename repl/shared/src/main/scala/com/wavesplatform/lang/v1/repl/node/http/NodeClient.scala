package com.wavesplatform.lang.v1.repl.node.http

import cats.Functor
import cats.implicits.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.repl.node.http.NodeClient.ResponseWrapper
import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse
import com.wavesplatform.lang.v1.repl.node.http.WebEnvironment.executionContext
import io.circe.Decoder
import io.circe.parser.decode
import com.wavesplatform.lang.v1.repl.Global

import scala.concurrent.Future

private[lang] trait NodeClient {
  def get[F[_]: Functor: ResponseWrapper, R: Decoder](path: String): Future[F[R]]
}

private[lang] case class NodeClientImpl(baseUrl: String) extends NodeClient {
  def get[F[_]: Functor: ResponseWrapper, R: Decoder](path: String): Future[F[R]] =
    Global
      .requestNode(baseUrl + path)
      .map(r => r: F[String])
      .map(_.map(decode[R]).map(_.explicitGet()))
}

private[lang] object NodeClient {
  type ResponseWrapper[F[_]] = NodeResponse => F[String]

  implicit val optionResponse: NodeResponse => Option[String] = {
    case NodeResponse(404, _)    => None
    case NodeResponse(200, body) => Some(body)
    case NodeResponse(_, body)   => throw new RuntimeException(body)
  }

  implicit val eitherResponse: NodeResponse => Either[String, String] = {
    case NodeResponse(200, body) => Right(body)
    case NodeResponse(_, body)   => Left(body)
  }

  implicit val idResponse: NodeResponse => String = {
    case NodeResponse(200, body) => body
    case NodeResponse(_, body)   => throw new RuntimeException(body)
  }
}
