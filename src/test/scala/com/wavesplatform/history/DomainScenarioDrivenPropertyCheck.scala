package com.wavesplatform.history

import com.wavesplatform.db.WithDomain
import com.wavesplatform.settings.WavesSettings
import org.scalacheck.Gen
import org.scalatest.{Assertion, Suite}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks => GeneratorDrivenPropertyChecks}

trait DomainScenarioDrivenPropertyCheck extends WithDomain { _: Suite with GeneratorDrivenPropertyChecks =>
  def scenario[S](gen: Gen[S], bs: WavesSettings = DefaultWavesSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen) { s =>
      withDomain(bs) { domain =>
        assertion(domain, s)
      }
    }
}
