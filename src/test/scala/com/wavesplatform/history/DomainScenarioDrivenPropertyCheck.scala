package com.wavesplatform.history

import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait DomainScenarioDrivenPropertyCheck extends GeneratorDrivenPropertyChecks {
  def scenario[S](gen: Gen[S])(assertion: (Domain, S) => Assertion): Assertion = forAll(gen)(assertion(domain(), _))
}
