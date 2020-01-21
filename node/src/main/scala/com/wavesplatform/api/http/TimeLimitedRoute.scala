package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.{Directive1, ExceptionHandler, Route}
import com.wavesplatform.utils.Schedulers
import monix.execution.Scheduler

import scala.concurrent.ExecutionException

trait TimeLimitedRoute { self: ApiRoute =>
  def limitedScheduler: Scheduler

  def executeLimited[T](f: => T): Directive1[T] = {
    val handler = ExceptionHandler {
      case _: InterruptedException | _: ExecutionException => complete(ApiError.CustomValidationError("Thread was interrupted"))
    }
    handleExceptions(handler) & onSuccess(Schedulers.executeOnTimeBoundedPool(limitedScheduler)(f))
  }

  def completeLimited(f: => ToResponseMarshallable): Route =
    executeLimited(f)(complete(_))
}
