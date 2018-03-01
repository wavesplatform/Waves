package com.wavesplatform.state2.diffs

import com.wavesplatform.db.WithState
import com.wavesplatform.state2._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.settings.TestFunctionalitySettings

class CommonValidationTimeTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink with WithState {

  property("disallows too old transacions") {
    forAll(for {
      prevBlockTs <- timestampGen
      blockTs <- timestampGen
      master <- accountGen
      height <- positiveIntGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      transfer1 = createWavesTransfer(master, recipient, amount, fee, prevBlockTs - CommonValidation.MaxTimePrevBlockOverTransactionDiff.toMillis - 1).explicitGet()
    } yield (prevBlockTs, blockTs, height, transfer1)) { case (prevBlockTs, blockTs, height, transfer1) =>
      withStateAndHistory(TestFunctionalitySettings.Enabled) { state =>
        TransactionDiffer(TestFunctionalitySettings.Enabled, Some(prevBlockTs), blockTs, height)(state, state, transfer1) should produce("too old")
      }
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
      transfer1 = createWavesTransfer(master, recipient, amount, fee, blockTs + CommonValidation.MaxTimeTransactionOverBlockDiff.toMillis + 1).explicitGet()
    } yield (prevBlockTs, blockTs, height, transfer1)) { case (prevBlockTs, blockTs, height, transfer1) =>
      val functionalitySettings = TestFunctionalitySettings.Enabled.copy(allowTransactionsFromFutureUntil = blockTs - 1)
      withStateAndHistory(functionalitySettings) { state =>
        TransactionDiffer(functionalitySettings, Some(prevBlockTs), blockTs, height)(state, state, transfer1) should produce("far future")
      }
    }
  }
}
