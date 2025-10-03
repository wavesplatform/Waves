package com.wavesplatform.state

import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings}

case class GenerationPeriod(start: Height, length: Int) {
  def next: GenerationPeriod = move(start + length)

  def prevOrThis: GenerationPeriod = {
    val prevStart = start - length
    if (prevStart >= 0) move(prevStart) else this
  }

  def max(other: GenerationPeriod): GenerationPeriod = if (start < other.start) other else this

  private def move(newStart: Int): GenerationPeriod = GenerationPeriod(Height(newStart), length)

  override def toString: String = s"GenerationPeriod(s=$start, l=$length)"
}

object GenerationPeriod {
  def from(h: Height, wavesSettings: WavesSettings): GenerationPeriod = from(h, wavesSettings.blockchainSettings.functionalitySettings)

  def from(h: Height, functionalitySettings: FunctionalitySettings): GenerationPeriod = {
    val l = functionalitySettings.generationPeriodLength
    GenerationPeriod(
      start = Height((h / l) * l),
      length = l
    )
  }
}
