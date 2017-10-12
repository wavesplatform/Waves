package com.wavesplatform.history

import com.wavesplatform.settings.WavesSettings
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait DomainScenarioDrivenPropertyCheck extends GeneratorDrivenPropertyChecks {
  def scenario[S](gen: Gen[S], bs: WavesSettings = DefaultWavesSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen)(assertion(domain(bs, AutoShutdownFeatureSettings), _))
}
