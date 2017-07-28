package com.wavesplatform.it


import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.it.api.NodeApi.UnexpectedStatusCodeException
import org.scalatest.{Assertion, Assertions}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

trait RequestErrorAssert extends Assertions {
  protected def assertBadRequest(f: Future[_]): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, r)) => Success(Assertions.assert(r.getStatusCode == StatusCodes.BadRequest))
    case Failure(e) => Success[Assertion](Assertions.fail(e))
    case _ => Success[Assertion](Assertions.fail(s"Expecting bad request"))
  }

  protected def assertBadRequestAndMessage(f: Future[_], message: String): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, r)) =>
      Success(Assertions.assert(r.getStatusCode == StatusCodes.BadRequest && r.getResponseBody.contains(message)))
    case Failure(e) => Success[Assertion](Assertions.fail(e))
    case _ => Success[Assertion](Assertions.fail(s"Expecting bad request"))
  }
}
