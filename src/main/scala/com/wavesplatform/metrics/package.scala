package com.wavesplatform

import kamon.metric.instrument.Histogram

package object metrics {
  final implicit class HistogramExt(val h: Histogram) extends AnyVal {
    def safeRecord(value: Long): Unit = h.record(Math.max(value, 0))
  }
}
