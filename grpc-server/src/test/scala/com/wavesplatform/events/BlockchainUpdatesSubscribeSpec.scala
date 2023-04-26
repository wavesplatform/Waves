package com.wavesplatform.events

import com.wavesplatform.TestValues.fee
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import org.scalatest.concurrent.ScalaFutures

import java.util.concurrent.ThreadLocalRandom.current

class BlockchainUpdatesSubscribeSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6
  val customFee: Long                = 1234567L
  val customAssetIssueFee            = 234567654L

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

    "return correct data for issue NFT tx" in {
      val issueSender              = TxHelpers.signer(58)
      val issueSenderBalanceBefore = 3.waves
      val name: String             = "Nft_test_asset"
      val description: String      = name + "__" + current.nextInt(1111, 999999)
      val issueNftTx = IssueTransaction
        .selfSigned(
          TxVersion.V3,
          issueSender,
          name,
          description,
          quantity = 1,
          decimals = 0,
          reissuable = false,
          script = None,
          0.001.waves,
          System.currentTimeMillis()
        )
        .explicitGet()

      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.ReduceNFTFee),
        balances = Seq(AddrWithBalance(issueSender.toAddress, issueSenderBalanceBefore))
      )(_.appendMicroBlock(issueNftTx)) { updates =>
        val append = updates(1).append
        checkIssue(append.transactionIds.head, append.transactionAt(0), issueNftTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (issueSender.toAddress, Waves)            -> (issueSenderBalanceBefore, issueSenderBalanceBefore - 0.001.waves),
            (issueSender.toAddress, issueNftTx.asset) -> (0, 1)
          )
        )
        checkAssetsBefore(append.transactionStateUpdates.head.assets, issueNftTx, isNft = true)
        checkAssetsAfter(append.transactionStateUpdates.head.assets, issueNftTx, isNft = true)
      }
    }

    "return correct data for reissue tx" in {
      val issueSender                = TxHelpers.signer(58)
      val senderBalanceBefore        = 4.waves
      val senderBalanceBeforeReissue = 3.waves
      val senderBalanceAfterReissue  = senderBalanceBeforeReissue - customAssetIssueFee
      val amount: Long               = current.nextInt(1, 9999999)
      val issueTx                    = TxHelpers.issue(issueSender, amount)
      val reissueTx                  = TxHelpers.reissue(issueTx.asset, issueSender, amount, reissuable = false, customAssetIssueFee)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(issueSender.toAddress, senderBalanceBefore))
      )(_.appendMicroBlock(issueTx, reissueTx)) { updates =>
        val append = updates(1).append
        checkReissue(append.transactionIds.apply(1), append.transactionAt(1), reissueTx)
        checkBalances(
          append.transactionStateUpdates.apply(1).balances,
          Map(
            (issueSender.toAddress, Waves)           -> (senderBalanceBeforeReissue, senderBalanceAfterReissue),
            (issueSender.toAddress, reissueTx.asset) -> (amount, amount * 2)
          )
        )
        checkAssetsBefore(append.transactionStateUpdates.head.assets, issueTx, isNft = false)
        checkAssetsAfter(append.transactionStateUpdates.head.assets, issueTx, isNft = false)
      }
    }

    "return correct data for burn tx" in {
      val issueSender                = TxHelpers.signer(58)
      val senderBalanceBefore        = 4.waves
      val senderBalanceBeforeReissue = 3.waves
      val senderBalanceAfterReissue  = senderBalanceBeforeReissue - customAssetIssueFee
      val amount: Long               = current.nextInt(1, 9999999)
      val issueTx                    = TxHelpers.issue(issueSender, amount)
      val burnTx                     = TxHelpers.burn(issueTx.asset, amount, issueSender, customAssetIssueFee)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(issueSender.toAddress, senderBalanceBefore))
      )(_.appendMicroBlock(issueTx, burnTx)) { updates =>
        val append = updates(1).append
        checkBurn(append.transactionIds.apply(1), append.transactionAt(1), burnTx)
        checkBalances(
          append.transactionStateUpdates.apply(1).balances,
          Map(
            (issueSender.toAddress, Waves)        -> (senderBalanceBeforeReissue, senderBalanceAfterReissue),
            (issueSender.toAddress, burnTx.asset) -> (amount, 0)
          )
        )
        checkAssetsBefore(append.transactionStateUpdates.head.assets, issueTx, isNft = false)
        checkAssetsAfter(append.transactionStateUpdates.head.assets, issueTx, isNft = false)
      }
    }

    "return correct data for exchange tx order V3, exchange V2" in {
      val buyer                       = TxHelpers.signer(58)
      val seller                      = TxHelpers.signer(189)
      val buyerBalanceBefore          = 4.waves
      val buyerBalanceBeforeExchange  = 3.waves
      val sellerBalanceBefore         = 4.waves
      val sellerBalanceBeforeExchange = 3.waves
      val priceAsset                  = TxHelpers.issue(buyer, 2000000000, 2)
      val priceAssetQuantity          = priceAsset.quantity.value
      val amountAsset                 = TxHelpers.issue(seller, 1000000000, 6)
      val amountAssetQuantity         = amountAsset.quantity.value

      val order1 = TxHelpers.order(
        OrderType.BUY,
        amountAsset.asset,
        priceAsset.asset,
        Waves,
        50000L,
        400000000L,
        fee = customFee,
        sender = buyer,
        matcher = buyer,
        version = Order.V3
      )
      val order2 = TxHelpers.order(
        OrderType.SELL,
        amountAsset.asset,
        priceAsset.asset,
        Waves,
        amount = 50000L,
        price = 400000000L,
        fee = customFee,
        sender = seller,
        matcher = buyer,
        version = Order.V3
      )
      val exchangedAssets = order1.price.value * order1.amount.value / 100000000
      val exchangeTx      = TxHelpers.exchangeFromOrders(order1, order2, buyer, version = TxVersion.V2)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(buyer.toAddress, buyerBalanceBefore),
          AddrWithBalance(seller.toAddress, sellerBalanceBefore)
        )
      )(_.appendMicroBlock(priceAsset, amountAsset, exchangeTx)) { updates =>
        val append = updates(1).append
        checkExchange(append.transactionIds.apply(2), append.transactionAt(2), exchangeTx)
        checkBalances(
          append.transactionStateUpdates.apply(2).balances,
          Map(
            (buyer.toAddress, Waves)              -> (buyerBalanceBeforeExchange, buyerBalanceBeforeExchange - fee + customFee),
            (seller.toAddress, priceAsset.asset)  -> (0, exchangedAssets),
            (buyer.toAddress, amountAsset.asset)  -> (0, order1.amount.value),
            (seller.toAddress, Waves)             -> (sellerBalanceBeforeExchange, sellerBalanceBeforeExchange - customFee),
            (buyer.toAddress, priceAsset.asset)   -> (priceAssetQuantity, priceAssetQuantity - exchangedAssets),
            (seller.toAddress, amountAsset.asset) -> (amountAssetQuantity, amountAssetQuantity - order1.amount.value)
          )
        )
      }
    }

    "return correct data for exchange tx order V4, exchange V3" in {
      val buyer                       = TxHelpers.signer(58)
      val seller                      = TxHelpers.signer(189)
      val buyerBalanceBefore          = 4.waves
      val buyerBalanceBeforeExchange  = 3.waves
      val sellerBalanceBefore         = 4.waves
      val sellerBalanceBeforeExchange = 3.waves
      val priceAsset                  = TxHelpers.issue(buyer, 2000000000, 4)
      val priceAssetQuantity          = priceAsset.quantity.value
      val amountAsset                 = TxHelpers.issue(seller, 1000000000, 6)
      val amountAssetQuantity         = amountAsset.quantity.value

      val order1 = TxHelpers.order(
        OrderType.BUY,
        amountAsset.asset,
        priceAsset.asset,
        Waves,
        50000L,
        400000000L,
        fee = customFee,
        sender = buyer,
        matcher = buyer,
        version = Order.V4
      )
      val order2 = TxHelpers.order(
        OrderType.SELL,
        amountAsset.asset,
        priceAsset.asset,
        Waves,
        amount = 50000L,
        price = 400000000L,
        fee = customFee,
        sender = seller,
        matcher = buyer,
        version = Order.V4
      )
      val exchangedAssets = order1.price.value * order1.amount.value / 10000000000L
      val exchangeTx      = TxHelpers.exchangeFromOrders(order1, order2, buyer, version = TxVersion.V3)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(buyer.toAddress, buyerBalanceBefore),
          AddrWithBalance(seller.toAddress, sellerBalanceBefore)
        )
      )(_.appendMicroBlock(priceAsset, amountAsset, exchangeTx)) { updates =>
        val append = updates(1).append
        checkExchange(append.transactionIds.apply(2), append.transactionAt(2), exchangeTx)
        checkBalances(
          append.transactionStateUpdates.apply(2).balances,
          Map(
            (buyer.toAddress, Waves)              -> (buyerBalanceBeforeExchange, buyerBalanceBeforeExchange - fee + customFee),
            (seller.toAddress, priceAsset.asset)  -> (0, exchangedAssets),
            (buyer.toAddress, amountAsset.asset)  -> (0, order1.amount.value),
            (seller.toAddress, Waves)             -> (sellerBalanceBeforeExchange, sellerBalanceBeforeExchange - customFee),
            (buyer.toAddress, priceAsset.asset)   -> (priceAssetQuantity, priceAssetQuantity - exchangedAssets),
            (seller.toAddress, amountAsset.asset) -> (amountAssetQuantity, amountAssetQuantity - order1.amount.value)
          )
        )
      }
    }

    "return correct data for lease tx" in {
      val sender              = TxHelpers.signer(33)
      val recipient           = TxHelpers.signer(123)
      val senderBalanceBefore = 10.waves
      val amount              = 5.waves
      val leaseTx             = TxHelpers.lease(sender, recipient.toAddress, amount, customFee)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(sender.toAddress, senderBalanceBefore))
      )(_.appendMicroBlock(leaseTx)) { updates =>
        val append = updates(1).append
        checkLease(append.transactionIds.head, append.transactionAt(0), leaseTx, recipient.toAddress.publicKeyHash)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (sender.toAddress, Waves) -> (senderBalanceBefore, senderBalanceBefore - customFee)
          )
        )
        checkLeasingForAddress(append.transactionStateUpdates.head.leasingForAddress, leaseTx)
        checkIndividualLeases(append.transactionStateUpdates.head.individualLeases, leaseTx, LeaseStatus.Active)
      }
    }
  }
}
