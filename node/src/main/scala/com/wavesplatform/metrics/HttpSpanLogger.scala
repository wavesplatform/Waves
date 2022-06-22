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

      val processingStart   = span.marks.find(_.key == ProcessingStartMark).map(_.instant)
      val akkaQueueDuration = processingStart.map(millisBetween(span.from, _))
      val processDuration =
        for {
          from <- processingStart
          to   <- span.marks.find(_.key == ResponseEndMark).map(_.instant)
        } yield millisBetween(from, to)
      val executorQueueDuration =
        for {
          from <- span.marks.find(_.key == ExecutorEnqueueMark).map(_.instant)
          to   <- span.marks.find(_.key == ExecutorStartMark).map(_.instant)
        } yield millisBetween(from, to)

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
    }
  }

  override def stop(): Unit = {}

  override def reconfigure(newConfig: Config): Unit = {}
}

object HttpSpanLogger {
  val ProcessingStartMark = "processing.start"
  val ResponseEndMark     = "http.response.ready"
  val ExecutorEnqueueMark = "executor.enqueue"
  val ExecutorStartMark   = "executor.start"

  case class Mark(key: String, duration: Double)

  def millisBetween(from: Instant, to: Instant): Double = Duration.between(from, to).toNanos * 1e-6
  implicit class FinishedSpanExt(val span: Span.Finished) extends AnyVal {
    def isAkkaHttpServer: Boolean = span.metricTags.get(Lookups.option("component")).contains("akka.http.server")
    def method: String            = span.metricTags.get(Lookups.plain(TagKeys.HttpMethod))
    def statusCode: Int           = span.metricTags.get(Lookups.plainLong(TagKeys.HttpStatusCode)).toInt
    def operation: String         = span.metricTags.get(Lookups.plain("operation"))
  }
}
