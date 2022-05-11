package com.wavesplatform.state.diffs

import com.wavesplatform.TestValues
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V1
import com.wavesplatform.settings.RewardsVotingSettings
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.test.{NumericExt, PropSpec}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{TxHelpers, TxValidationError, TxVersion}

class TransferTransactionDiffTest extends PropSpec with WithDomain {

  property("transfers assets to recipient preserving waves invariant") {
    val sender    = TxHelpers.secondAddress
    val senderKp  = TxHelpers.secondSigner
    val recipient = TxHelpers.address(2)
    val feeDiff   = Diff(portfolios = Map(sender -> Portfolio.waves(TestValues.fee)))

    withDomain(DomainPresets.mostRecent.copy(rewardsSettings = RewardsVotingSettings(None)), AddrWithBalance.enoughBalances(senderKp)) { d =>
      val wavesTransfer = TxHelpers.transfer(senderKp, recipient)
      assertBalanceInvariant(d.createDiff(wavesTransfer).combineF(feeDiff).explicitGet())

      d.appendAndAssertSucceed(wavesTransfer)
      d.blockchain.balance(recipient) shouldBe wavesTransfer.amount.value
      d.blockchain.balance(sender) shouldBe ENOUGH_AMT - wavesTransfer.amount.value - wavesTransfer.fee.value
    }

    withDomain(DomainPresets.mostRecent, AddrWithBalance.enoughBalances(senderKp)) { d =>
      val asset         = d.helpers.issueAsset(senderKp)
      val assetTransfer = TxHelpers.transfer(senderKp, recipient, asset = asset, amount = 1000)
      assertBalanceInvariant(d.createDiff(assetTransfer).combineF(feeDiff).explicitGet())

      d.appendAndAssertSucceed(assetTransfer)
      d.blockchain.balance(recipient) shouldBe 0L
      d.blockchain.balance(recipient, asset) shouldBe 1000L
      d.blockchain.balance(sender) shouldBe ENOUGH_AMT - assetTransfer.fee.value - 1.waves
      d.blockchain.balance(sender, asset) shouldBe 0L
    }
  }

  property("handle transactions with amount + fee > Long.MaxValue") {
    withDomain(DomainPresets.ScriptsAndSponsorship, AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val asset    = d.helpers.issueAsset(amount = Long.MaxValue)
      val transfer = TxHelpers.transfer(asset = asset, amount = Long.MaxValue, version = TxVersion.V1, fee = 100000)
      d.appendAndCatchError(transfer) shouldBe TransactionDiffer.TransactionValidationError(TxValidationError.OverflowError, transfer)
    }

    withDomain(DomainPresets.mostRecent, AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val asset    = d.helpers.issueAsset(amount = Long.MaxValue)
      val transfer = TxHelpers.transfer(asset = asset, amount = Long.MaxValue, version = TxVersion.V1, fee = 100000)
      d.appendAndAssertSucceed(transfer)
    }
  }

  property("fails, if smart asset used as a fee") {
    withDomain(DomainPresets.mostRecent, AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val asset    = d.helpers.issueAsset(script = TxHelpers.exprScript(V1)("true"), amount = 100000000)
      val transfer = TxHelpers.transfer(feeAsset = asset)

      val diffOrError = TransferTransactionDiff(d.blockchain)(transfer)
      diffOrError shouldBe Left(GenericError("Smart assets can't participate in TransferTransactions as a fee"))
    }
  }
}
