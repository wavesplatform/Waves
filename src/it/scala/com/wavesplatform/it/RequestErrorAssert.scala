package com.wavesplatform.it

import com.wavesplatform.it.Node.UnexpectedStatusCodeException
import org.scalatest.{Assertion, Assertions}

import scala.concurrent.Future
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

trait RequestErrorAssert extends Assertions {
  protected def assertRequestError(f: Future[_]): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(r)) => Success(Assertions.assert(r.getStatusCode == 400))
    case Failure(e) => Failure[Assertion](new RuntimeException(s"Unexpected state: ${e.getMessage}", e))
  }
}
