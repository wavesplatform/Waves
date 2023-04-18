package com.wavesplatform.events.blockchainUpdateTests

import com.wavesplatform.events.blockchainUpdateTests.fixtures.{SubscribeHandler, TxStateUpdateChecks, WavesTxChecks}
import com.wavesplatform.events.WithBUDomain
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.transaction.TxHelpers.secondAddress
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = RideV6
  val customFee: Long                = 1234567L

  "BlockchainUpdates subscribe tests" - {
    "return correct data for alias tx" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      val testAlias: String                      = "test"
      val prepare: Prepare                       = Prepare(d, repo, customFee)
      val aliasTx: CreateAliasTransaction        = prepare.createAliasTransaction(testAlias)
      val aliasTxId: Array[Byte]                 = aliasTx.id.value().arr
      val handler: SubscribeHandler              = prepare.appendedSubscriptionState(aliasTxId, 2, 0)
      val append: Append                         = handler.getAppend
      val txIndex: Int                           = handler.getTxIndex
      val txCheckers: WavesTxChecks              = WavesTxChecks(append, txIndex)
      val txStateUpdateInfo: TxStateUpdateChecks = TxStateUpdateChecks(append)

      txCheckers.baseTxCheckers(aliasTx, txIndex)
      txCheckers.checkAliasTx(aliasTx)
      txStateUpdateInfo.balancesCheckers(
        0,
        secondAddress,
        prepare.getSenderBalanceBefore(),
        prepare.getSenderBalanceAfter(),
        _
      )
    }

    "return correct data for transfer tx" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      val amount: Long                           = 1000L
      val prepare: Prepare                       = Prepare(d, repo, customFee)
      val transferTx: TransferTransaction        = prepare.createTransferTransaction(amount, Waves)
      val transferTxId                           = transferTx.id.value().arr
      val handler: SubscribeHandler              = prepare.appendedSubscriptionState(transferTxId, 2, 0)
      val append: Append                         = handler.getAppend
      val txIndex: Int                           = handler.getTxIndex
      val txCheckers: WavesTxChecks              = WavesTxChecks(append, handler.getTxIndex)
      val txStateUpdateInfo: TxStateUpdateChecks = TxStateUpdateChecks(append)

      txCheckers.baseTxCheckers(transferTx, txIndex)
      txCheckers.checkTransferTx(amount, prepare.recipientAddress)
      txStateUpdateInfo.balancesCheckers(0, secondAddress, prepare.getSenderBalanceBefore(), prepare.getSenderBalanceAfter(amount), _)
      txStateUpdateInfo.balancesCheckers(1, prepare.recipientAddress, prepare.getRecipientBalanceBefore(), prepare.getRecipientBalanceAfter(amount), _)
    }
  }
}
