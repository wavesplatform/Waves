package com.wavesplatform.events.blockchainupdatetests

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.*
import com.wavesplatform.events.blockchainupdatetests.fixtures.WavesTxChecks.*
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeSpec extends FreeSpec with WithBUDomain with ScalaFutures {
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
          Map((aliasSender.toAddress, Waves) -> (aliasSenderBalanceBefore, aliasSenderBalanceBefore - customFee))
        )
      }
    }

    "return correct data for transfer tx" in {
      val amount: Long                   = 1000L
      val transferSender                 = TxHelpers.signer(155)
      val transferSenderBalanceBefore    = 5.waves
      val transferSenderBalanceAfter     = transferSenderBalanceBefore - customFee - amount
      val transferRecipient              = TxHelpers.signer(123)
      val recipientAddress               = transferRecipient.toAddress
      val transferRecipientBalanceBefore = 1.waves
      val transferRecipientBalanceAfter  = transferRecipientBalanceBefore + amount
      val transferTx                     = TxHelpers.transfer(transferSender, recipientAddress, amount, Waves, customFee)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(transferSender.toAddress, transferSenderBalanceBefore),
          AddrWithBalance(transferRecipient.toAddress, transferRecipientBalanceBefore)
        )
      )(_.appendMicroBlock(transferTx)) { updates =>
        val append = updates(1).append
        checkTransfer(append.transactionIds.head, append.transactionAt(0), transferTx, recipientAddress.publicKeyHash)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (transferSender.toAddress, Waves) -> (transferSenderBalanceBefore, transferSenderBalanceAfter),
            (recipientAddress, Waves)         -> (transferRecipientBalanceBefore, transferRecipientBalanceAfter)
          )
        )
      }
    }
  }
}
