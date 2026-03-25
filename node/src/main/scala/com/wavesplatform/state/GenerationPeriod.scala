package com.wavesplatform.state

import cats.syntax.option.*
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings}
import com.wavesplatform.state.GenerationPeriod.*

case class GenerationPeriod(activation: Height, start: Height, length: Int) extends Ordered[GenerationPeriod] {
  require(start >= activation, s"GenerationPeriod: $start >= $activation")

  def end: Height = {
    val offset = if (isZero) 0 else -1
    start + length + offset
  }

  def next: GenerationPeriod = move(end + 1)

  def prev: Option[GenerationPeriod] = {
    if (isZero) none
    else {
      val prevStart = start - length
      if (prevStart < activation + length) zeroPeriod(activation, length).some
      else move(prevStart).some
    }
  }

  def max(other: GenerationPeriod): GenerationPeriod = if (start < other.start) other else this

  private def isZero: Boolean = activation == start

  private def move(newStart: Height): GenerationPeriod = GenerationPeriod(activation, newStart, length)

  override def compare(that: GenerationPeriod): Int = start compare that.start

  override def toString: String = s"[$start, $end]"
}

object GenerationPeriod {
  def from(h: Height, activation: Height, wavesSettings: WavesSettings): Option[GenerationPeriod] =
    from(h, activation, wavesSettings.blockchainSettings.functionalitySettings)

  def from(h: Height, activation: Height, functionalitySettings: FunctionalitySettings): Option[GenerationPeriod] =
    from(h, activation, functionalitySettings.generationPeriodLength)

  /** First period starts from: activation + generationPeriodLength + [1; generationPeriodLength] */
  def from(h: Height, activation: Height, generationPeriodLength: Int): Option[GenerationPeriod] =
    if (h < activation) none
    else {
      val blockAfterActivation = h - activation
      val periodIndex          = (blockAfterActivation.toInt - 1) / generationPeriodLength
      GenerationPeriod(
        activation,
        if (periodIndex == 0) activation else activation + periodIndex * generationPeriodLength + 1,
        generationPeriodLength
      ).some
    }

  def enclosedPeriods(
      activation: Height,
      generationPeriodLength: Int,
      start: Height,
      end: Height
  ): Option[(start: GenerationPeriod, end: GenerationPeriod)] = {
    val fromGenerationPeriod =
      from(start, activation, generationPeriodLength).flatMap(_.prev).getOrElse(zeroPeriod(activation, generationPeriodLength))

    if (fromGenerationPeriod.start > end) none
    else
      from(end, activation, generationPeriodLength).map { endPeriod =>
        (fromGenerationPeriod, endPeriod.next.max(fromGenerationPeriod.next))
      }
  }

  private def zeroPeriod(activation: Height, generationPeriodLength: Int): GenerationPeriod =
    GenerationPeriod(activation, activation, generationPeriodLength)
}
