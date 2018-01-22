package com.wavesplatform.it

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.it.RequestErrorAssert.ErrorMessage
import com.wavesplatform.it.api.UnexpectedStatusCodeException
import org.scalatest.{Assertion, Assertions}
import play.api.libs.json.Json.parse
import play.api.libs.json.{Format, Json}
import scorex.api.http.ApiErrorResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait RequestErrorAssert extends Assertions {
  protected def assertBadRequest(f: Future[_]): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, statusCode, _)) => Success(Assertions.assert(statusCode == StatusCodes.BadRequest.intValue))
    case Failure(e) => Success(Assertions.fail(e))
    case _ => Success(Assertions.fail(s"Expecting bad request"))
  }

  protected def expectErrorResponse(f: Future[_])(isExpected: ApiErrorResponse => Boolean): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      val parsedError = Json.parse(responseBody).validate[ApiErrorResponse].asOpt
      parsedError match {
        case None => Success(Assertions.fail(s"Expecting bad request"))
        case Some(err) => Success(Assertions.assert(statusCode == StatusCodes.BadRequest.intValue && isExpected(err)))
      }
    case Failure(e) => Success(Assertions.fail(e))
    case _ => Success(Assertions.fail(s"Expecting bad request"))
  }

  protected def assertBadRequestAndMessage(f: Future[_], errorMessage: String): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) => {
      Success(Assertions.assert(statusCode == StatusCodes.BadRequest.intValue && parse(responseBody).as[ErrorMessage].message.contains(errorMessage)))
    }
    case Failure(e) => Success[Assertion](Assertions.fail(e))
    case _ => Success[Assertion](Assertions.fail(s"Expecting bad request"))
  }

}

object RequestErrorAssert {

  case class ErrorMessage(error: Int, message: String)

  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

}
