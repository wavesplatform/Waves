package com.wavesplatform.api.http

import akka.http.scaladsl.server._
import com.wavesplatform.utils.ScorexLogging
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import play.api.libs.json.JsObject

trait CustomDirectives extends Directives with ApiMarshallers with ScorexLogging {
  def anyParam(paramName: String, nonEmpty: Boolean = false, limit: Int = Int.MaxValue): Directive1[Iterable[String]] = {
    val baseDirective = (get & parameter(paramName.as[String].*).map(_.toSeq.reverse)) |
      post & (formField(paramName.as[String].*) |
        entity(as[JsObject]).map { jso =>
          (jso \ s"${paramName}s").as[Iterable[String]]
        })

    baseDirective
      .flatMap {
        case list if nonEmpty && list.isEmpty => reject(MissingQueryParamRejection(paramName))
        case list if list.size > limit      => complete(ApiError.TooBigArrayAllocation(limit))
        case list                             => provide(list)
      }
  }

  def extractScheduler: Directive1[Scheduler] =
    extractExecutionContext.map(ec => Scheduler(ec, UncaughtExceptionReporter((t: Throwable) => log.debug("Error processing request", t))))
}
