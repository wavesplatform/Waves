package com.wavesplatform.metrics

import com.wavesplatform.database.Key
import kamon.metric.{HistogramMetric, MeasurementUnit}

object LevelDBStats {
  implicit class DbHistogramExt(val h: HistogramMetric) {
    def recordTagged(key: Key[_], value: Array[Byte]): Unit = recordTagged(key.name, value)

    def recordTagged(tag: String, value: Array[Byte]): Unit =
      h.refine("key", tag).record(Option(value).map(_.length.toLong).getOrElse(0))

    def recordTagged(tag: String, totalBytes: Long): Unit =
      h.refine("key", tag).record(totalBytes)
  }

  val miss  = WavesKamon.histogram("node.db.cachemiss")
  val read  = WavesKamon.histogram("node.db.read", MeasurementUnit.information.bytes)
  val write = WavesKamon.histogram("node.db.write", MeasurementUnit.information.bytes)
}
