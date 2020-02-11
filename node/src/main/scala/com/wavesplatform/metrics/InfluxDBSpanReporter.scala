package com.wavesplatform.metrics

import com.typesafe.config.Config
import com.wavesplatform.utils.ScorexLogging
import kamon.module.{ModuleFactory, SpanReporter}
import kamon.tag.Tag
import kamon.trace.Span
import org.influxdb.dto.Point

class InfluxDBSpanReporterFactory extends ModuleFactory {
  override def create(settings: ModuleFactory.Settings): InfluxDBSpanReporter =
    new InfluxDBSpanReporter
}

class InfluxDBSpanReporter extends SpanReporter with ScorexLogging {
  override def reportSpans(spans: Seq[Span.Finished]): Unit = spans.foreach { span =>
    val basePoint = Point
      .measurement(span.operationName)
      .addField("finish", span.to.toEpochMilli - span.from.toEpochMilli)

    val pointWithTags = (span.tags.all() ++ span.metricTags.all())
      .foldLeft(basePoint)(
        (bp, t) =>
          bp.tag(t.key, Tag.unwrapValue(t) match {
            case s: String => s.take(6)
            case v         => String.valueOf(v)
          })
      )

    val pointWithMarks = span.marks.foldLeft(pointWithTags) { (bp, m) =>
      val timeDiff = m.instant.toEpochMilli - span.from.toEpochMilli
      bp.addField(m.key, timeDiff)
    }
    log.info(s"Span written: $span")
    Metrics.write(pointWithMarks, span.from.toEpochMilli)
  }

  override def stop(): Unit                         = ()
  override def reconfigure(newConfig: Config): Unit = ()
}
