package com.wavesplatform.state

import com.wavesplatform.settings.FunctionalitySettings

case class GenerationPeriod(start: Height, period: Int) {
  def next: GenerationPeriod = move(start + period)

  def prevOrThis: GenerationPeriod = {
    val prevStart = start - period
    if (prevStart >= 0) move(prevStart) else this
  }

  def max(other: GenerationPeriod): GenerationPeriod = if (start < other.start) other else this

  private def move(newStart: Int): GenerationPeriod = GenerationPeriod(Height(newStart), period)

  override def toString: String = s"GenerationPeriod(s=$start, p=$period)"
}

object GenerationPeriod {
  def from(h: Height, functionalitySettings: FunctionalitySettings): GenerationPeriod = {
    val commitmentPeriod = functionalitySettings.generationPeriod
    GenerationPeriod(
      start = Height((h / commitmentPeriod) * commitmentPeriod),
      period = commitmentPeriod
    )
  }
}
