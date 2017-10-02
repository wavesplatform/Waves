package com.wavesplatform.metrics

import kamon.metric.instrument.Histogram

final class HistogramExt(val h: Histogram) extends AnyVal {
  def safeRecord(value: Long): Unit = h.record(Math.max(value, 0))
}
