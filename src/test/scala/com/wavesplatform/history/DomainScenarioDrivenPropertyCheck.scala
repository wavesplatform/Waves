package com.wavesplatform.history

import com.wavesplatform.settings.BlockchainSettings
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait DomainScenarioDrivenPropertyCheck extends GeneratorDrivenPropertyChecks {
  def scenario[S](gen: Gen[S], bs: BlockchainSettings = DefaultBlockchainSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen)(assertion(domain(bs, EmptyFeaturesSettings), _))
}
