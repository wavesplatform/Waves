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
      .foldLeft(basePoint)((bp, t) => bp.tag(t.key, String.valueOf(Tag.unwrapValue(t))))

    val (pointWithMarks, _) = span.marks.foldRight((pointWithTags, span.from.toEpochMilli)) {
      case (m, (bp, lastMarkTime)) =>
        val currentTime  = m.instant.toEpochMilli
        val relativeDiff = currentTime - lastMarkTime
        (
          bp.addField(m.key, relativeDiff),
          currentTime
        )
    }
    // log.trace(s"Span written: $span")
    Metrics.write(pointWithMarks, span.from.toEpochMilli)
  }

  override def stop(): Unit                         = ()
  override def reconfigure(newConfig: Config): Unit = ()
}
