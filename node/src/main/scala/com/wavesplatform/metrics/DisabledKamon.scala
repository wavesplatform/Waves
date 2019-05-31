package com.wavesplatform.metrics

import java.time.Duration

import kamon.metric._
import kamon.{JTags, Tags}

private[metrics] object DisabledKamon extends MetricLookup {
  override def histogram(name: String, unit: MeasurementUnit, dynamicRange: Option[DynamicRange]): HistogramMetric = new HistogramMetric {
    override def name: String = ""
    override def refine(tags: JTags): Histogram = this
    override def refine(tags: Tags): Histogram = this
    override def refine(tags: (String, String)*): Histogram = this
    override def refine(tag: String, value: String): Histogram = this
    override def remove(tags: JTags): Boolean = true
    override def remove(tags: Tags): Boolean = true
    override def remove(tags: (String, String)*): Boolean = true
    override def remove(tag: String, value: String): Boolean = true
    override def unit: MeasurementUnit = MeasurementUnit.none
    override def dynamicRange: DynamicRange = DynamicRange.Default
    override def record(value: Long): Unit = ()
    override def record(value: Long, times: Long): Unit = ()
  }

  override def timer(name: String, dynamicRange: Option[DynamicRange]): TimerMetric = new TimerMetric {
    override def name: String = ""
    override def refine(tags: JTags): Timer = this
    override def refine(tags: Tags): Timer = this
    override def refine(tags: (String, String)*): Timer = this
    override def refine(tag: String, value: String): Timer = this
    override def remove(tags: JTags): Boolean = true
    override def remove(tags: Tags): Boolean = true
    override def remove(tags: (String, String)*): Boolean = true
    override def remove(tag: String, value: String): Boolean = true
    override def start(): StartedTimer = () => ()
    override def unit: MeasurementUnit = MeasurementUnit.none
    override def dynamicRange: DynamicRange = DynamicRange.Default
    override def record(value: Long): Unit = ()
    override def record(value: Long, times: Long): Unit = ()
  }

  override def counter(name: String, unit: MeasurementUnit): CounterMetric = new CounterMetric {
    override def name: String = ""
    override def refine(tags: JTags): Counter = this
    override def refine(tags: Tags): Counter = this
    override def refine(tags: (String, String)*): Counter = this
    override def refine(tag: String, value: String): Counter = this
    override def remove(tags: JTags): Boolean = true
    override def remove(tags: Tags): Boolean = true
    override def remove(tags: (String, String)*): Boolean = true
    override def remove(tag: String, value: String): Boolean = true
    override def unit: MeasurementUnit = MeasurementUnit.none
    override def increment(): Unit = ()
    override def increment(times: Long): Unit = ()
  }

  override def gauge(name: String, unit: MeasurementUnit): GaugeMetric = new GaugeMetric {
    override def name: String = ""
    override def refine(tags: JTags): Gauge = this
    override def refine(tags: Tags): Gauge = this
    override def refine(tags: (String, String)*): Gauge = this
    override def refine(tag: String, value: String): Gauge = this
    override def remove(tags: JTags): Boolean = true
    override def remove(tags: Tags): Boolean = true
    override def remove(tags: (String, String)*): Boolean = true
    override def remove(tag: String, value: String): Boolean = true
    override def unit: MeasurementUnit = MeasurementUnit.none
    override def increment(): Unit = ()
    override def increment(times: Long): Unit = ()
    override def decrement(): Unit = ()
    override def decrement(times: Long): Unit = ()
    override def set(value: Long): Unit = ()
  }

  override def rangeSampler(name: String, unit: MeasurementUnit, sampleInterval: Option[Duration], dynamicRange: Option[DynamicRange]): RangeSamplerMetric = new RangeSamplerMetric {
    override def name: String = ""
    override def refine(tags: JTags): RangeSampler = this
    override def refine(tags: Tags): RangeSampler = this
    override def refine(tags: (String, String)*): RangeSampler = this
    override def refine(tag: String, value: String): RangeSampler = this
    override def remove(tags: JTags): Boolean = true
    override def remove(tags: Tags): Boolean = true
    override def remove(tags: (String, String)*): Boolean = true
    override def remove(tag: String, value: String): Boolean = true
    override def unit: MeasurementUnit = MeasurementUnit.none
    override def dynamicRange: DynamicRange = DynamicRange.Default
    override def sampleInterval: Duration = Duration.ZERO
    override def increment(): Unit = ()
    override def increment(times: Long): Unit = ()
    override def decrement(): Unit = ()
    override def decrement(times: Long): Unit = ()
    override def sample(): Unit = ()
  }
}
