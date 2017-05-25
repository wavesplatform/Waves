package com.wavesplatform.it

import com.wavesplatform.it.Node.UnexpectedStatusCodeException
import org.scalatest.{Assertion, Assertions}

import scala.concurrent.Future
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

trait RequestErrorAssert extends Assertions {
  protected def assertBadRequest(f: Future[_]): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, r)) => Success(Assertions.assert(r.getStatusCode == 400))
    case Failure(e) => Success[Assertion](Assertions.fail(e))
    case _ => Success[Assertion](Assertions.fail(s"Expecting bad request"))
  }
}
