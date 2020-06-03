package com.wavesplatform.api.http

import akka.http.scaladsl.server.{Directive1, _}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.{Scheduler, UncaughtExceptionReporter}

trait CustomDirectives extends Directives with ScorexLogging {
  def extractScheduler: Directive1[Scheduler] =
    extractExecutionContext.map(ec => Scheduler(ec, UncaughtExceptionReporter((t: Throwable) => log.debug("Error processing request", t))))
}
