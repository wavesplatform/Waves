package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Directive1, _}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import play.api.libs.json.JsObject

trait CustomDirectives extends Directives with ApiMarshallers with ScorexLogging {
  def anyParam(paramName: String): Directive1[Iterable[String]] =
    (get & parameter(paramName.as[String].*).map(_.toSeq.reverse)) |
      post & (formField(paramName.as[String].*) |
        entity(as[JsObject]).map { jso =>
          (jso \ s"${paramName}s").as[Iterable[String]]
        })

  def extractScheduler: Directive1[Scheduler] =
    extractExecutionContext.map(ec => Scheduler(ec, UncaughtExceptionReporter((t: Throwable) => log.debug("Error processing request", t))))
}
