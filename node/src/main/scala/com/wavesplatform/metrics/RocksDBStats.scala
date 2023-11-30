package com.wavesplatform.metrics

import com.wavesplatform.database.Key
import kamon.Kamon
import kamon.metric.{MeasurementUnit, Metric}

//noinspection TypeAnnotation
object RocksDBStats {
  implicit class DbHistogramExt(private val h: Metric.Histogram) extends AnyVal {
    def recordTagged(key: Key[?], value: Array[Byte]): Unit = recordTagged(key.name, value)

    def recordTagged(tag: String, value: Array[Byte]): Unit =
      h.withTag("key", tag).record(Option(value).fold(0L)(_.length))

    def recordTagged(tag: String, totalBytes: Long): Unit =
      h.withTag("key", tag).record(totalBytes)
  }

  val miss  = Kamon.histogram("node.db.cachemiss").withoutTags()
  val read  = Kamon.histogram("node.db.read", MeasurementUnit.information.bytes)
  val write = Kamon.histogram("node.db.write", MeasurementUnit.information.bytes)
}
