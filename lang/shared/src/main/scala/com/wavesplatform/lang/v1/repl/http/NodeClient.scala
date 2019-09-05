package com.wavesplatform.lang.v1.repl.http

import cats.Functor
import cats.implicits._
import com.softwaremill.sttp.{Response, sttp, _}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.repl.global.sttpBackend

import io.circe.Decoder
import io.circe.parser.decode
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.higherKinds

private[http] case class NodeClient(baseUrl: String) {
  implicit val fastRunExecutionContext: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
  }

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
    val header = List(("User-Agent", "Chrome"))

    val request = sttp.get(uri"$url").copy(
      headers = header,
      options = sttp.options.copy(readTimeout = timeout)
    )
    await(request.send(), timeout)
  }

  // for scala.js compatibility
  private def await[A](f: Future[A], timeout: Duration): A = {
    val endTime = System.currentTimeMillis() + timeout.toMillis
    @tailrec def awaitRec(f: Future[A]): A =
      if (f.isCompleted) {
        var result: Option[A] = None
        f.foreach(r => { result = Some(r) })
        result.get
      }
      else if (System.currentTimeMillis() > endTime)
        throw new RuntimeException("Connection timeout")
      else awaitRec(f)
    awaitRec(f)
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
