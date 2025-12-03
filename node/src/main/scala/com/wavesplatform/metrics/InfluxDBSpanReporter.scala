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
    def createPoint(name: String): Point.Builder =
      (span.tags.all() ++ span.metricTags.all())
        .foldLeft(Point.measurement(name))((bp, t) => bp.tag(t.key, String.valueOf(Tag.unwrapValue(t))))

    val (pointWithMarks, _) = span.marks.foldRight((createPoint(span.operationName), span.from.toEpochMilli)) { case (m, (bp, lastMarkTime)) =>
      val currentTime  = m.instant.toEpochMilli
      val relativeDiff = currentTime - lastMarkTime
      (
        bp.addField(m.key, relativeDiff),
        currentTime
      )
    }
    Metrics.write(pointWithMarks, span.from.toEpochMilli)

    val timeMarks = ("span.started" -> span.from) +: span.marks.reverse.map(m => (m.key, m.instant)) :+ ("span.finished" -> span.to)
    timeMarks.foreach { case (name, time) =>
      val point = createPoint(s"${span.operationName}.marks").tag("mark", name)
      Metrics.write(point, time.toEpochMilli)
    }
  }

  override def stop(): Unit                         = ()
  override def reconfigure(newConfig: Config): Unit = ()
}
