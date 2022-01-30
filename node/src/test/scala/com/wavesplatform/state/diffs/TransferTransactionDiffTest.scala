package com.wavesplatform.state.diffs

import cats.syntax.monoid._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.{TxHelpers, TxValidationError, TxVersion}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.TestValues
import com.wavesplatform.lang.directives.values.StdLibVersion

class TransferTransactionDiffTest extends PropSpec with WithDomain {

  property("transfers assets to recipient preserving waves invariant") {
    val feeDiff = Diff(portfolios = Map(TxHelpers.defaultAddress -> Portfolio.waves(TestValues.fee)))

    withDomain(DomainPresets.mostRecent) { d =>
      d.helpers.creditWavesToDefaultSigner()

      val wavesTransfer = TxHelpers.transfer()
      assertBalanceInvariant(d.createDiff(wavesTransfer) |+| feeDiff)

      d.appendAndAssertSucceed(wavesTransfer)
      d.blockchain.balance(TxHelpers.secondAddress) shouldBe wavesTransfer.amount
      d.blockchain.balance(TxHelpers.defaultAddress) shouldBe 899400000L
    }

    withDomain(DomainPresets.mostRecent) { d =>
      d.helpers.creditWavesToDefaultSigner()

      val asset         = d.helpers.issueAsset()
      val assetTransfer = TxHelpers.transfer(asset = asset, amount = 1000)
      assertBalanceInvariant(d.createDiff(assetTransfer) |+| feeDiff)

      d.appendAndAssertSucceed(assetTransfer)
      d.blockchain.balance(TxHelpers.secondAddress) shouldBe 0L
      d.blockchain.balance(TxHelpers.secondAddress, asset) shouldBe 1000L
      d.blockchain.balance(TxHelpers.defaultAddress) shouldBe 999400000L
      d.blockchain.balance(TxHelpers.defaultAddress, asset) shouldBe 0L
    }
  }

  property("handle transactions with amount + fee > Long.MaxValue") {
    withDomain(DomainPresets.ScriptsAndSponsorship) { d =>
      d.helpers.creditWavesToDefaultSigner()
      val asset    = d.helpers.issueAsset(amount = Long.MaxValue)
      val transfer = TxHelpers.transfer(asset = asset, amount = Long.MaxValue, version = TxVersion.V1, fee = 100000)
      d.appendAndCatchError(transfer) shouldBe TransactionDiffer.TransactionValidationError(TxValidationError.OverflowError, transfer)
    }

    withDomain(DomainPresets.mostRecent) { d =>
      d.helpers.creditWavesToDefaultSigner()
      val asset    = d.helpers.issueAsset(amount = Long.MaxValue)
      val transfer = TxHelpers.transfer(asset = asset, amount = Long.MaxValue, version = TxVersion.V1, fee = 100000)
      d.appendAndAssertSucceed(transfer)
    }
  }

  property("fails, if smart asset used as a fee") {
    withDomain(DomainPresets.mostRecent) { d =>
      d.helpers.creditWavesToDefaultSigner(Long.MaxValue)
      val asset    = d.helpers.issueAsset(script = TxHelpers.exprScript(StdLibVersion.V1)("true"), amount = 100000000)
      val transfer = TxHelpers.transfer(feeAsset = asset)

      val diffOrError = TransferTransactionDiff(d.blockchain, System.currentTimeMillis())(transfer)
      diffOrError shouldBe Left(GenericError("Smart assets can't participate in TransferTransactions as a fee"))
    }
  }
}
