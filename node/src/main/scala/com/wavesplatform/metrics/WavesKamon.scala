package com.wavesplatform.metrics

import java.time.Duration

import kamon.Kamon
import kamon.metric._

private[wavesplatform] object WavesKamon extends MetricLookup {
  @volatile private[this] var kamonRef: MetricLookup = Kamon
  @volatile private[this] var enabled = true

  def isEnabled: Boolean = this.enabled

  def disable(): Unit = {
    kamonRef = DisabledKamon
    enabled = false
  }

  override def histogram(name: String): HistogramMetric = kamonRef.histogram(name)
  override def histogram(name: String, unit: MeasurementUnit): HistogramMetric = kamonRef.histogram(name, unit)
  override def histogram(name: String, unit: MeasurementUnit, dynamicRange: DynamicRange): HistogramMetric = kamonRef.histogram(name, unit, dynamicRange)
  override def timer(name: String): TimerMetric = kamonRef.timer(name)
  override def timer(name: String, dynamicRange: DynamicRange): TimerMetric = kamonRef.timer(name, dynamicRange)
  override def counter(name: String): CounterMetric = kamonRef.counter(name)
  override def gauge(name: String): GaugeMetric = kamonRef.gauge(name)
  override def rangeSampler(name: String): RangeSamplerMetric = kamonRef.rangeSampler(name)
  override def rangeSampler(name: String, unit: MeasurementUnit): RangeSamplerMetric = kamonRef.rangeSampler(name, unit)
  override def rangeSampler(name: String, unit: MeasurementUnit, sampleInterval: Duration): RangeSamplerMetric = kamonRef.rangeSampler(name, unit, sampleInterval)
  override def rangeSampler(name: String, unit: MeasurementUnit, sampleInterval: Duration, dynamicRange: DynamicRange): RangeSamplerMetric = kamonRef.rangeSampler(name, unit, sampleInterval, dynamicRange)
  override def histogram(name: String, unit: MeasurementUnit, dynamicRange: Option[DynamicRange]): HistogramMetric = kamonRef.histogram(name, unit, dynamicRange)
  override def timer(name: String, dynamicRange: Option[DynamicRange]): TimerMetric = kamonRef.timer(name, dynamicRange)
  override def counter(name: String, unit: MeasurementUnit): CounterMetric = kamonRef.counter(name, unit)
  override def gauge(name: String, unit: MeasurementUnit): GaugeMetric = kamonRef.gauge(name, unit)
  override def rangeSampler(name: String, unit: MeasurementUnit, sampleInterval: Option[Duration], dynamicRange: Option[DynamicRange]): RangeSamplerMetric = kamonRef.rangeSampler(name, unit, sampleInterval, dynamicRange)
}
