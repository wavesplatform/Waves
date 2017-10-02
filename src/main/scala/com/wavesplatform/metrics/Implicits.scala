package com.wavesplatform.metrics

import kamon.metric.instrument.Histogram

object Implicits {
  implicit def toHistogramExt(h: Histogram): HistogramExt = new HistogramExt(h)
}
