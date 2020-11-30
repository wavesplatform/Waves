package com.wavesplatform.metrics

import java.time.{Duration, Instant}

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import kamon.instrumentation.tag.TagKeys
import kamon.module.{ModuleFactory, SpanReporter}
import kamon.tag.Lookups
import kamon.trace.Span

class HttpSpanLoggerFactory extends ModuleFactory {
  override def create(settings: ModuleFactory.Settings): HttpSpanLogger = new HttpSpanLogger
}

class HttpSpanLogger extends SpanReporter with LazyLogging {
  import HttpSpanLogger._
  override def reportSpans(spans: Seq[Span.Finished]): Unit = logger.whenInfoEnabled {
    for (span <- spans if span.isAkkaHttpServer) {
      val code = StatusCodes.getForKey(span.statusCode).fold("<unknown>")(c => c.toString())
      val timeline = span.marks.reverse
        .foldLeft((span.from, Seq.newBuilder[String])) {
          case ((prevInstant, builder), m) =>
            m.instant -> (builder += f"${m.key}: +${millisBetween(prevInstant, m.instant)}%.3f")
        }
        ._2
        .result()
        .mkString("[", ", ", "]")
      logger.info(
        f"${span.trace.id.string} ${span.method} ${span.operation}: $code in ${millisBetween(span.from, span.to)}%.3f ms $timeline"
      )
    }
  }

  override def stop(): Unit = {}

  override def reconfigure(newConfig: Config): Unit = {}
}

object HttpSpanLogger {
  def millisBetween(from: Instant, to: Instant): Double = Duration.between(from, to).toNanos * 1e-6
  implicit class FinishedSpanExt(val span: Span.Finished) extends AnyVal {
    def isAkkaHttpServer: Boolean = span.metricTags.get(Lookups.option("component")).contains("akka.http.server")
    def method: String            = span.metricTags.get(Lookups.plain(TagKeys.HttpMethod))
    def statusCode: Int           = span.metricTags.get(Lookups.plainLong(TagKeys.HttpStatusCode)).toInt
    def operation: String         = span.metricTags.get(Lookups.plain("http.request_uri"))
  }
}
