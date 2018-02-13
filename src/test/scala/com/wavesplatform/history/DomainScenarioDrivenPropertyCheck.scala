package com.wavesplatform.history

import com.wavesplatform.settings.WavesSettings
import org.iq80.leveldb.DB
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait DomainScenarioDrivenPropertyCheck extends GeneratorDrivenPropertyChecks {
  def scenario[S](gen: Gen[S], db: DB, bs: WavesSettings = DefaultWavesSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen)(assertion(domain(db, bs), _))
}
