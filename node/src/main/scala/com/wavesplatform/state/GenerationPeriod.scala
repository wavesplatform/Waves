package com.wavesplatform.state

import com.wavesplatform.settings.FunctionalitySettings

case class GenerationPeriod(start: Height, period: Int) {
  def next: GenerationPeriod = GenerationPeriod(Height(start + period), period)
}

object GenerationPeriod {
  def from(h: Height, functionalitySettings: FunctionalitySettings): GenerationPeriod = {
    val commitmentPeriod = functionalitySettings.commitmentPeriod
    GenerationPeriod(
      start = Height((h / commitmentPeriod) * commitmentPeriod),
      period = commitmentPeriod
    )
  }
}
