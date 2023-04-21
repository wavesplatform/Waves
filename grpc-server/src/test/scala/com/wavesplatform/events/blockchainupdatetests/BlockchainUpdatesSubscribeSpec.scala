package com.wavesplatform.events.blockchainupdatetests

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.*
import com.wavesplatform.events.blockchainupdatetests.fixtures.WavesTxChecks.*
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.assets.IssueTransaction
import org.scalatest.concurrent.ScalaFutures

import java.util.concurrent.ThreadLocalRandom.current

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
      )(_.appendMicroBlock(aliasTx)) { updates =>
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

    "return correct data for issue tx" in {
      val customAssetIssueFee      = 234567654L
      val issueSender              = TxHelpers.signer(58)
      val issueSenderBalanceBefore = 10.waves
      val script                   = Option(TxHelpers.script("true"))
      val amount: Long             = current.nextInt(1, 9999999)
      val decimals: Byte           = current.nextInt(0, 8).toByte
      val name: String             = "Test_asset"
      val description: String      = name + "|_|_|_|_|_|" + current.nextInt(1111111, 9999999)
      val issueTx                  = TxHelpers.issue(issueSender, amount, decimals, name, description, customAssetIssueFee, script)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(issueSender.toAddress, issueSenderBalanceBefore))
      )(_.appendMicroBlock(issueTx)) { updates =>
        val append = updates(1).append
        checkIssue(append.transactionIds.head, append.transactionAt(0), issueTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (issueSender.toAddress, Waves)         -> (issueSenderBalanceBefore, issueSenderBalanceBefore - customAssetIssueFee),
            (issueSender.toAddress, issueTx.asset) -> (0, amount)
          )
        )
        checkAssetsBefore(append.transactionStateUpdates.head.assets, issueTx, isNft = false)
        checkAssetsAfter(append.transactionStateUpdates.head.assets, issueTx, isNft = false)
      }
    }
  }
}
