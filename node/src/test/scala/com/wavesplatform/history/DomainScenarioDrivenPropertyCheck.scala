package com.wavesplatform.history

import com.wavesplatform.db.WithDomain
import com.wavesplatform.settings.WavesSettings
import org.scalacheck.Gen
import org.scalatest.Suite
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks => GeneratorDrivenPropertyChecks}

trait DomainScenarioDrivenPropertyCheck extends WithDomain { _: Suite with GeneratorDrivenPropertyChecks =>
  def scenario[S](gen: Gen[S], bs: WavesSettings = DefaultWavesSettings)(assertion: (Domain, S) => Any): Any =
    forAll(gen) { s =>
      withDomain(bs) { domain =>
        assertion(domain, s)
      }
    }
}
