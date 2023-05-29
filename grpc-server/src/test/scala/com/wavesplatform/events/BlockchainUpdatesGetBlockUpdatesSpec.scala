package com.wavesplatform.events

import com.wavesplatform.TestValues.fee
import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.api.grpc.protobuf.GetBlockUpdateResponse
import com.wavesplatform.events.fixtures.WavesTxChecks.{checkExchange, *}
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.{Asset, TxHelpers, TxVersion}
import org.scalatest.concurrent.ScalaFutures

import java.util.concurrent.ThreadLocalRandom.current

class BlockchainUpdatesGetBlockUpdatesSpec extends BlockchainUpdatesTestBase {

  "BlockchainUpdates getBlockUpdate tests" - {
    "BU- . Return correct data for alias from getBlockUpdate" in {
      val aliasTx = TxHelpers.createAlias("test", sender, fee = customFee)
      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendBlock(aliasTx)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkAlias(append, aliasTx)
      }
    }

    "BU- . Return correct data for transfer" in {
      val amount: Long                   = 1000L
      val transferSenderBalanceAfter     = senderBalanceBefore - customFee - amount
      val transferRecipient              = TxHelpers.signer(3)
      val recipientAddress               = transferRecipient.toAddress
      val transferRecipientBalanceBefore = 1.waves
      val transferRecipientBalanceAfter  = transferRecipientBalanceBefore + amount
      val transferTx                     = TxHelpers.transfer(sender, recipientAddress, amount, Waves, customFee)

      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(senderAddress, senderBalanceBefore),
          AddrWithBalance(transferRecipient.toAddress, transferRecipientBalanceBefore)
        )
      )(_.appendBlock(transferTx)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkTransfer(append.transactionIds.head, append.transactionAt(0), transferTx, recipientAddress.publicKeyHash)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (senderAddress, Waves)    -> (senderBalanceBefore, transferSenderBalanceAfter),
            (recipientAddress, Waves) -> (transferRecipientBalanceBefore, transferRecipientBalanceAfter)
          )
        )
      }
    }

    "BU- . Return correct data for issue" in {
      val script              = Option(TxHelpers.script("true"))
      val amount: Long        = current.nextInt(1, 9999999)
      val decimals: Byte      = current.nextInt(0, 8).toByte
      val name: String        = "Test_asset"
      val description: String = name + "|_|_|_|_|_|" + current.nextInt(1111111, 9999999)
      val issue               = TxHelpers.issue(sender, amount, decimals, name, description, customAssetIssueFee, script)
      val issueScript         = issue.script.get.bytes.value().arr

      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendBlock(issue)) { getBlockUpdate =>
        val append       = getBlockUpdate.getUpdate.getAppend
        val assetDetails = append.transactionStateUpdates.head.assets.head

        checkIssue(append.transactionIds.head, append.transactionAt(0), issue)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (senderAddress, Waves)       -> (senderBalanceBefore, senderBalanceBefore - customAssetIssueFee),
            (senderAddress, issue.asset) -> (0, amount)
          )
        )
        checkAssetsStateUpdates(assetDetails.after, issue, isNft = false, issue.quantity.value)
        checkAssetsScriptStateUpdates(assetDetails.after.get.scriptInfo, issueScript)
      }
    }

    "BU- . Return correct data for issue NFT" in {
      val name: String        = "Nft_test_asset"
      val description: String = name + "__" + current.nextInt(1111, 999999)
      val issueNftTx = IssueTransaction
        .selfSigned(
          TxVersion.V3,
          sender,
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

      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings.addFeatures(BlockchainFeatures.ReduceNFTFee),
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendBlock(issueNftTx)) { getBlockUpdate =>
        val append       = getBlockUpdate.getUpdate.getAppend
        val assetDetails = append.transactionStateUpdates.head.assets.head

        checkIssue(append.transactionIds.head, append.transactionAt(0), issueNftTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (senderAddress, Waves)            -> (senderBalanceBefore, senderBalanceBefore - 0.001.waves),
            (senderAddress, issueNftTx.asset) -> (0, 1)
          )
        )
        checkAssetsStateUpdates(assetDetails.after, issueNftTx, isNft = true, issueNftTx.quantity.value)
      }
    }

    "BU- . Return correct data for reissue" in {
      val amount: Long               = 9000000
      val amountReissue: Long        = 7500000
      val issue                      = TxHelpers.issue(sender, amount)
      val reissueTx                  = TxHelpers.reissue(issue.asset, sender, amountReissue, reissuable = false, customAssetIssueFee)
      val quantityAfterReissue       = amount + amountReissue
      val senderBalanceBeforeReissue = senderBalanceBefore - issue.fee.value
      val senderBalanceAfterReissue  = senderBalanceBeforeReissue - reissueTx.fee.value

      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(reissueTx)
      } { getBlockUpdate =>
        val append       = getBlockUpdate.getUpdate.getAppend
        val assetDetails = append.transactionStateUpdates.head.assets.head

        checkReissue(append.transactionIds.head, append.transactionAt(0), reissueTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (senderAddress, Waves)           -> (senderBalanceBeforeReissue, senderBalanceAfterReissue),
            (senderAddress, reissueTx.asset) -> (amount, quantityAfterReissue)
          )
        )
        checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
        checkAssetsStateUpdates(assetDetails.after, reissueTx, isNft = false, quantityAfterReissue)
      }
    }

    "Exchange transaction subscription tests" - {
      val buyer = TxHelpers.signer(58)
      val seller = TxHelpers.signer(189)
      val buyerBalanceBefore = 4.waves
      val buyerBalanceBeforeExchange = 3.waves
      val sellerBalanceBefore = 4.waves
      val sellerBalanceBeforeExchange = 3.waves
      val priceAsset = TxHelpers.issue(buyer, 2000000000, 2)
      val priceAssetQuantity = priceAsset.quantity.value
      val amountAsset = TxHelpers.issue(seller, 1000000000, 6)
      val amountAssetQuantity = amountAsset.quantity.value

      "BU- . Return correct data for order V3, exchange V2" in {
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
        val exchangeTx = TxHelpers.exchangeFromOrders(order1, order2, buyer, version = TxVersion.V2)
        addedBlocksAndSubscribe(exchangeTx) { getBlockUpdate =>
          checkingSubscribeFields(getBlockUpdate.getUpdate.getAppend, exchangeTx, exchangedAssets, order1.amount.value)
        }
      }

      "BU- . Return correct data for order V4, exchange V3" in {
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
        val exchangedAssets = order1.price.value / 2 / 10000000
        val exchangeTx = TxHelpers.exchangeFromOrders(order1, order2, buyer, version = TxVersion.V3)
        addedBlocksAndSubscribe(exchangeTx) { getBlockUpdate =>
          checkingSubscribeFields(getBlockUpdate.getUpdate.getAppend, exchangeTx, exchangedAssets, order1.amount.value)
        }
      }

      def addedBlocksAndSubscribe(exchangeTx: ExchangeTransaction)(f: GetBlockUpdateResponse => Unit): Unit = {
        withGenerateGetBlockUpdate(
          height = 4,
          settings = currentSettings,
          balances = Seq(
            AddrWithBalance(buyer.toAddress, buyerBalanceBefore),
            AddrWithBalance(seller.toAddress, sellerBalanceBefore)
          )
        ) { d =>
          d.appendBlock(priceAsset)
          d.appendBlock(amountAsset)
          d.appendBlock(exchangeTx)
        }(f)
      }

      def checkingSubscribeFields(append: Append, exchangeTx: ExchangeTransaction, exchangedAssets: Long, orderAmount: Long): Unit = {
        checkExchange(append.transactionIds.head, append.transactionAt(0), exchangeTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (buyer.toAddress, Waves) -> (buyerBalanceBeforeExchange, buyerBalanceBeforeExchange - fee + customFee),
            (seller.toAddress, priceAsset.asset) -> (0, exchangedAssets),
            (buyer.toAddress, amountAsset.asset) -> (0, orderAmount),
            (seller.toAddress, Waves) -> (sellerBalanceBeforeExchange, sellerBalanceBeforeExchange - customFee),
            (buyer.toAddress, priceAsset.asset) -> (priceAssetQuantity, priceAssetQuantity - exchangedAssets),
            (seller.toAddress, amountAsset.asset) -> (amountAssetQuantity, amountAssetQuantity - orderAmount)
          )
        )
      }
    }

    "BU- . Return correct data for lease" in {
      val recipient = TxHelpers.signer(123)
      val recipientAddress = recipient.toAddress
      val amount = 5.waves
      val lease = TxHelpers.lease(sender, recipientAddress, amount, customFee)
      val leaseId = lease.id.value().arr

      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendBlock(lease)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkLease(append.transactionIds.head, append.transactionAt(0), lease, recipientAddress.publicKeyHash)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (senderAddress, Waves) -> (senderBalanceBefore, senderBalanceBefore - customFee)
          )
        )
        checkLeasingForAddress(
          append.transactionStateUpdates.head.leasingForAddress,
          Map(
            (senderAddress, 0L, amount) -> (0L, 0L),
            (recipientAddress, amount, 0L) -> (0L, 0L)
          )
        )
        checkIndividualLeases(
          append.transactionStateUpdates.head.individualLeases,
          Map(
            (LeaseStatus.Active, amount) -> (leaseId, lease.sender.arr, lease.recipient.bytes, leaseId)
          )
        )
      }
    }

    "BU- . Return correct data for lease cancel" in {
      val recipient = TxHelpers.signer(123)
      val recipientAddress = recipient.toAddress
      val amount = 5.waves
      val lease = TxHelpers.lease(sender, recipientAddress, amount, customFee)
      val leaseCancel = TxHelpers.leaseCancel(lease.id.value(), sender, customFee)
      val leaseId = leaseCancel.leaseId.arr
      val senderBalanceBeforeTx = senderBalanceBefore - lease.fee.value
      val senderBalanceAfterTx = senderBalanceBeforeTx - leaseCancel.fee.value

      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(lease)
        d.appendMicroBlock(leaseCancel)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkLeaseCancel(append.transactionIds.head, append.transactionAt(0), leaseCancel)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (senderAddress, Waves) -> (senderBalanceBeforeTx, senderBalanceAfterTx)
          )
        )
        checkLeasingForAddress(
          append.transactionStateUpdates.head.leasingForAddress,
          Map(
            (senderAddress, 0L, 0L) -> (0L, amount),
            (recipientAddress, 0L, 0L) -> (amount, 0L)
          )
        )
        checkIndividualLeases(
          append.transactionStateUpdates.head.individualLeases,
          Map(
            (LeaseStatus.Inactive, amount) -> (leaseId, lease.sender.arr, lease.recipient.bytes, leaseId)
          )
        )
      }
    }


  }
}
