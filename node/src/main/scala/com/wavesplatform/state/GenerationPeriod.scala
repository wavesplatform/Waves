package com.wavesplatform.state

import com.wavesplatform.settings.FunctionalitySettings

case class GenerationPeriod(start: Height, period: Int) {
  def next: GenerationPeriod = GenerationPeriod(Height(start + period), period)

  def max(other: GenerationPeriod): GenerationPeriod = if (start < other.start) other else this

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
