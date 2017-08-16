package com.wavesplatform.it

import java.nio.charset.StandardCharsets

import com.wavesplatform.it.api.NodeApi.UnexpectedStatusCodeException
import org.scalatest.{Assertion, Assertions}
import play.api.libs.json.Json
import scorex.api.http.ApiErrorResponse

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

trait RequestErrorAssert extends Assertions {
  protected def assertBadRequest(f: Future[_]): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, r)) => Success(Assertions.assert(r.getStatusCode == 400))
    case Failure(e) => Success(Assertions.fail(e))
    case _ => Success(Assertions.fail(s"Expecting bad request"))
  }

  protected def expectErrorResponse(f: Future[_])(isExpected: ApiErrorResponse => Boolean): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, r)) =>
      val parsedError = Json.parse(r.getResponseBody(StandardCharsets.UTF_8)).validate[ApiErrorResponse].asOpt
      parsedError match {
        case None => Success(Assertions.fail(s"Expecting bad request"))
        case Some(err) => Success(Assertions.assert(r.getStatusCode == 400 && isExpected(err)))
      }
    case Failure(e) => Success(Assertions.fail(e))
    case _ => Success(Assertions.fail(s"Expecting bad request"))
  }
}
