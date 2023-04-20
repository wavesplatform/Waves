package com.wavesplatform.events.blockchainUpdateTests

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.*
import com.wavesplatform.events.blockchainUpdateTests.fixtures.{SubscribeHandler, TxStateUpdateChecks, WavesTxChecks}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.secondAddress
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  import WavesTxChecks.*

  val currentSettings: WavesSettings = DomainPresets.RideV6
  val customFee: Long                = 1234567L
  val recipient: KeyPair             = TxHelpers.signer(1234)
  val recipientAddress: Address      = recipient.toAddress

  "BlockchainUpdates subscribe tests" - {
    "return correct data for alias tx" in {
      val aliasSender              = TxHelpers.signer(125)
      val aliasSenderBalanceBefore = 100.waves
      val aliasTx                  = TxHelpers.createAlias("test", sender = aliasSender, fee = customFee)
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(aliasSender.toAddress, aliasSenderBalanceBefore))
      )(_.appendBlock(aliasTx)) { updates =>
        val append = updates(1).append
        checkCreateAlias(append.transactionIds.head, append.transactionAt(0), aliasTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(aliasSender.toAddress -> (Waves, aliasSenderBalanceBefore, aliasSenderBalanceBefore - customFee))
        )
      }
    }

    "return correct data for transfer tx" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      val amount: Long                           = 1000L
      val prepare: Prepare                       = Prepare(d, repo, customFee)
      val senderBalanceBefore                    = d.balance(secondAddress)
      val recipientBalanceBefore                 = d.balance(recipientAddress)
      val transferTx: TransferTransaction        = prepare.createTransferTransaction(recipientAddress, amount, Waves)
      val transferTxId                           = transferTx.id.value().arr
      val handler: SubscribeHandler              = prepare.appendedSubscriptionState(transferTxId, 2, 0)
      val append: Append                         = handler.getAppend
      val txIndex: Int                           = handler.getTxIndex
      val txCheckers: WavesTxChecks              = WavesTxChecks(append, handler.getTxIndex)
      val txStateUpdateInfo: TxStateUpdateChecks = TxStateUpdateChecks(append)

      txCheckers.baseTxCheckers(transferTx, txIndex)
      txCheckers.checkTransferTx(amount, recipientAddress)
      txStateUpdateInfo.balancesCheckers(0, secondAddress, senderBalanceBefore, d.balance(secondAddress), _)
      txStateUpdateInfo.balancesCheckers(1, recipientAddress, recipientBalanceBefore, d.balance(recipientAddress), _)
    }
  }
}
