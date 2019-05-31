package com.wavesplatform.metrics

import java.time.Duration

import kamon.Kamon
import kamon.metric._

private[wavesplatform] object WavesKamon extends MetricLookup {
  @volatile
  private[this] var KamonRef: MetricLookup = Kamon

  def disable(): Unit = {
    KamonRef = DisabledKamon
  }

  override def histogram(name: String): HistogramMetric = KamonRef.histogram(name)
  override def histogram(name: String, unit: MeasurementUnit): HistogramMetric = KamonRef.histogram(name, unit)
  override def histogram(name: String, unit: MeasurementUnit, dynamicRange: DynamicRange): HistogramMetric = KamonRef.histogram(name, unit, dynamicRange)
  override def timer(name: String): TimerMetric = KamonRef.timer(name)
  override def timer(name: String, dynamicRange: DynamicRange): TimerMetric = KamonRef.timer(name, dynamicRange)
  override def counter(name: String): CounterMetric = KamonRef.counter(name)
  override def gauge(name: String): GaugeMetric = KamonRef.gauge(name)
  override def rangeSampler(name: String): RangeSamplerMetric = KamonRef.rangeSampler(name)
  override def rangeSampler(name: String, unit: MeasurementUnit): RangeSamplerMetric = KamonRef.rangeSampler(name, unit)
  override def rangeSampler(name: String, unit: MeasurementUnit, sampleInterval: Duration): RangeSamplerMetric = KamonRef.rangeSampler(name, unit, sampleInterval)
  override def rangeSampler(name: String, unit: MeasurementUnit, sampleInterval: Duration, dynamicRange: DynamicRange): RangeSamplerMetric = KamonRef.rangeSampler(name, unit, sampleInterval, dynamicRange)
  override def histogram(name: String, unit: MeasurementUnit, dynamicRange: Option[DynamicRange]): HistogramMetric = KamonRef.histogram(name, unit, dynamicRange)
  override def timer(name: String, dynamicRange: Option[DynamicRange]): TimerMetric = KamonRef.timer(name, dynamicRange)
  override def counter(name: String, unit: MeasurementUnit): CounterMetric = KamonRef.counter(name, unit)
  override def gauge(name: String, unit: MeasurementUnit): GaugeMetric = KamonRef.gauge(name, unit)
  override def rangeSampler(name: String, unit: MeasurementUnit, sampleInterval: Option[Duration], dynamicRange: Option[DynamicRange]): RangeSamplerMetric = KamonRef.rangeSampler(name, unit, sampleInterval, dynamicRange)
}
