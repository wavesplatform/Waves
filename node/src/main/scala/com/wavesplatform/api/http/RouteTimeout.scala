package com.wavesplatform.api.http

import akka.NotUsed
import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.server.Directives.{complete, handleExceptions, withExecutionContext}
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.stream.scaladsl.Source
import com.typesafe.scalalogging.LazyLogging
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.ExecutionContext.fromExecutor
import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

class RouteTimeout(timeout: FiniteDuration)(implicit sc: Scheduler) extends ApiMarshallers with LazyLogging {
  private val ece = fromExecutor(sc, t => logger.warn(s"Exception in RouteTimeout", t))
  private val handler = ExceptionHandler { case _: TimeoutException =>
    complete(ApiError.ServerRequestTimeout)
  }

  def executeToFuture[T](task: Task[T])(implicit m: ToResponseMarshaller[T]): Route =
    execute(task)(_.runToFuture(_))

  def executeStreamed[T, R](task: Task[Seq[T]])(f: T => R)(implicit m: ToResponseMarshaller[Source[R, NotUsed]]): Route =
    execute(task) { (task, sc) =>
      task
        .runToFuture(sc)
        .map(Source(_).map(f))(sc)
    }

  def executeFromObservable[T](observable: Observable[T])(implicit m: ToResponseMarshaller[Source[T, NotUsed]]): Route =
    withExecutionContext(ece) {
      handleExceptions(handler) &
        complete(Source.fromPublisher(observable.toReactivePublisher(sc)).initialTimeout(timeout))
    }

  def execute[T](task: Task[T])(f: (Task[T], Scheduler) => ToResponseMarshallable): Route = {
    withExecutionContext(ece) {
      handleExceptions(handler) & complete(f(task.timeout(timeout), sc))
    }
  }
}
