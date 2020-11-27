package com.wavesplatform.metrics

import java.time.{Duration, Instant, LocalDateTime, ZoneId}
import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import kamon.instrumentation.tag.TagKeys
import kamon.module.{ModuleFactory, SpanReporter}
import kamon.tag.Lookups
import kamon.trace.Span
import play.api.libs.json.{JsObject, Json}

class HttpSpanLoggerFactory extends ModuleFactory {
  override def create(settings: ModuleFactory.Settings): HttpSpanLogger = new HttpSpanLogger
}

class HttpSpanLogger extends SpanReporter with LazyLogging {
  import HttpSpanLogger.*
  override def reportSpans(spans: Seq[Span.Finished]): Unit = logger.whenInfoEnabled {
    for (span <- spans if span.isAkkaHttpServer) {
      val code = StatusCodes.getForKey(span.statusCode).fold("<unknown>")(c => c.toString())

      val processingStart   = span.marks.find(_.key == "processing.start").map(_.instant)
      val akkaQueueDuration = processingStart.map(millisBetween(span.from, _))
      val processDuration =
        for {
          from <- processingStart
          to   <- span.marks.find(_.key == "http.response.ready").map(_.instant)
        } yield millisBetween(from, to)
      val executorQueueDuration =
        for {
          from <- span.marks.find(_.key == "executor.enqueue").map(_.instant)
          to   <- span.marks.find(_.key == "executor.start").map(_.instant)
        } yield millisBetween(from, to)
//      val timeline = span.marks.reverse
//        .foldLeft((span.from, List.empty[Mark])) { case ((prevInstant, acc), m) =>
//          m.instant -> (Mark(m.key, millisBetween(prevInstant, m.instant)) +: acc)
//        }
//        ._2
      val extraFields = akkaQueueDuration.toSeq.map("akka_queue_duration" -> Json.toJson(_)) ++
        processDuration.toSeq.map("process_duration" -> Json.toJson(_)) ++
        executorQueueDuration.toSeq.map("executor_queue_duration" -> Json.toJson(_))
      val json = Json.obj(
        "@timestamp" -> LocalDateTime.ofInstant(span.from, ZoneId.systemDefault()),
        "trace_id"   -> span.trace.id.string,
        "method"     -> span.method,
        "path"       -> span.operation,
        "code"       -> code,
        "duration"   -> millisBetween(span.from, span.to)
      ) ++ JsObject(extraFields)

      logger.info(json.toString)
//      logger.info(
//        f"${span.trace.id.string} ${span.method} ${span.operation}: $code in ${millisBetween(span.from, span.to)}%.3f ms $timeline"
//      )
    }
  }

  override def stop(): Unit = {}

  override def reconfigure(newConfig: Config): Unit = {}
}

object HttpSpanLogger {
  case class Mark(key: String, duration: Double)

  def millisBetween(from: Instant, to: Instant): Double = Duration.between(from, to).toNanos * 1e-6
  implicit class FinishedSpanExt(val span: Span.Finished) extends AnyVal {
    def isAkkaHttpServer: Boolean = span.metricTags.get(Lookups.option("component")).contains("akka.http.server")
    def method: String            = span.metricTags.get(Lookups.plain(TagKeys.HttpMethod))
    def statusCode: Int           = span.metricTags.get(Lookups.plainLong(TagKeys.HttpStatusCode)).toInt
    def operation: String         = span.metricTags.get(Lookups.plain("operation")) // span.metricTags.get(Lookups.plain("http.request_uri"))
  }
}
