package com.wavesplatform.state2.diffs

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.PaymentTransaction

class CommonValidationTimeTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  property("disallows too old transacions") {
    forAll(for {
      prevBlockTs <- timestampGen
      blockTs <- timestampGen
      master <- accountGen
      height <- positiveIntGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      transfer1 = PaymentTransaction.create(master, recipient, amount, fee, prevBlockTs - CommonValidation.MaxTimePrevBlockOverTransactionDiff.toMillis - 1).explicitGet()
    } yield (prevBlockTs, blockTs, height, transfer1)) { case (prevBlockTs, blockTs, height, transfer1) =>
      TransactionDiffer(TestFunctionalitySettings.Enabled, Some(prevBlockTs), blockTs, height)(newState(), transfer1) should produce("too old")
    }
  }

  property("disallows transactions from far future") {
    forAll(for {
      prevBlockTs <- timestampGen
      blockTs <- Gen.choose(prevBlockTs, prevBlockTs + 7 * 24 * 3600 * 1000)
      master <- accountGen
      height <- positiveIntGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      transfer1 = PaymentTransaction.create(master, recipient, amount, fee, blockTs + CommonValidation.MaxTimeTransactionOverBlockDiff.toMillis + 1).explicitGet()
    } yield (prevBlockTs, blockTs, height, transfer1)) { case (prevBlockTs, blockTs, height, transfer1) =>
      val functionalitySettings = TestFunctionalitySettings.Enabled.copy(allowTransactionsFromFutureUntil = blockTs - 1)
      TransactionDiffer(functionalitySettings, Some(prevBlockTs), blockTs, height)(newState(), transfer1) should produce("far future")
    }
  }
}
