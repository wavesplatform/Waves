package com.wavesplatform.metrics

import com.wavesplatform.database.Key
import kamon.Kamon
import kamon.metric.{Histogram, MeasurementUnit}

object LevelDBStats {
  implicit class DbHistogramExt(val h: Histogram) {
    def recordTagged(key: Key[_], value: Array[Byte]): Unit = recordTagged(key.name, value)

    def recordTagged(tag: String, value: Array[Byte]): Unit =
      h.withTag("key", tag).record(Option(value).map(_.length.toLong).getOrElse(0))

    def recordTagged(tag: String, totalBytes: Long): Unit =
      h.withTag("key", tag).record(totalBytes)
  }

  val miss  = Kamon.histogram("node.db.cachemiss").withoutTags()
  val read  = Kamon.histogram("node.db.read", MeasurementUnit.information.bytes).withoutTags()
  val write = Kamon.histogram("node.db.write", MeasurementUnit.information.bytes).withoutTags()
}
