package com.wavesplatform.utils

import java.util.concurrent.TimeUnit

import scala.concurrent.duration._

object DebugUtils {
  final class Measure(name: String, start: Long) {
    def elapsed: FiniteDuration   = (System.nanoTime() - start).nanos
    override def toString: String = f"${name.capitalize} took ${elapsed.toUnit(TimeUnit.SECONDS)}%.2fs"
  }

  def start(name: String): Measure           = new Measure(name, System.nanoTime())
  def startMulti(name: String): MultiMeasure = new MultiMeasure(name)

  final class MultiMeasure(name: String) {
    @volatile var measures = Vector.empty[FiniteDuration]

    def elapsed: FiniteDuration = measures.foldLeft(Duration.Zero)(_ + _)
    def average: FiniteDuration = (elapsed.toNanos / measures.size).nanos

    def measureOperation[T](f: => T): T = {
      val start  = DebugUtils.start(name)
      val result = f
      measures :+= start.elapsed
      result
    }

    def startOperation(): Long = System.nanoTime()

    def finishOperation(timestamp: Long): Unit = {
      measures :+= new Measure(name, timestamp).elapsed
    }

    override def toString: String =
      f"${name.capitalize}: ${measures.size} ops, average ${average.toMillis}ms, total ${elapsed.toUnit(TimeUnit.SECONDS)}%.2fs"
  }
}
