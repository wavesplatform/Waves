package com.wavesplatform.events

import com.wavesplatform.TestValues.fee
import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
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

class BlockchainUpdatesSubscribeSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6
  val customFee: Long                = 5234000L
  val customAssetIssueFee            = 234560000L
  val sender: SeedKeyPair            = TxHelpers.signer(12)
  val senderAddress: Address         = sender.toAddress
  val senderBalanceBefore: Long      = 20.waves
  val testScript: Script = TxHelpers.script(s"""{-# STDLIB_VERSION 6 #-}
                                               |{-# CONTENT_TYPE DAPP #-}
                                               |{-# SCRIPT_TYPE ACCOUNT #-}
                                               |
                                               |@Verifier(tx)
                                               |func verify () = match(tx) {
                                               |    case _ =>
                                               |      if (
                                               |        ${(1 to 9).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || \n")}
                                               |      ) then true else true
                                               |}""".stripMargin)

  "BlockchainUpdates subscribe tests" - {
    "BU-1. Return correct data for alias" in {
      val aliasTx                    = TxHelpers.createAlias("test", sender, fee = customFee)
      val senderBalanceAfterTx: Long = senderBalanceBefore - aliasTx.fee.value

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendMicroBlock(aliasTx)) { updates =>
        val append = updates(1).append
        checkCreateAlias(append.transactionIds.head, append.transactionAt(0), aliasTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map((senderAddress, Waves) -> (senderBalanceBefore, senderBalanceAfterTx))
        )
      }
    }

    "BU-28. Return correct data for transfer" in {
      val amount: Long                   = 1000L
      val transferSenderBalanceAfter     = senderBalanceBefore - customFee - amount
      val transferRecipient              = TxHelpers.signer(123)
      val recipientAddress               = transferRecipient.toAddress
      val transferRecipientBalanceBefore = 1.waves
      val transferRecipientBalanceAfter  = transferRecipientBalanceBefore + amount
      val transferTx                     = TxHelpers.transfer(sender, recipientAddress, amount, Waves, customFee)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(senderAddress, senderBalanceBefore),
          AddrWithBalance(transferRecipient.toAddress, transferRecipientBalanceBefore)
        )
      )(_.appendMicroBlock(transferTx)) { updates =>
        val append = updates(1).append
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

    "Return correct data for several transfers to miner" in {
      val amount      = 1000
      val recipient   = TxHelpers.defaultAddress
      val fee1        = customFee
      val fee2        = customFee + 12345
      val fee3        = customFee + 56789
      val transferTx1 = TxHelpers.transfer(sender, recipient, amount, Waves, fee1)
      val transferTx2 = TxHelpers.transfer(sender, recipient, amount, Waves, fee2)
      val transferTx3 = TxHelpers.transfer(sender, recipient, amount, Waves, fee3)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(senderAddress, senderBalanceBefore)
        )
      )(_.appendMicroBlock(transferTx1, transferTx2, transferTx3)) { updates =>
        val append = updates(1).append
        checkTransfer(append.transactionIds(0), append.transactionAt(0), transferTx1, recipient.publicKeyHash)
        checkTransfer(append.transactionIds(1), append.transactionAt(1), transferTx2, recipient.publicKeyHash)
        checkTransfer(append.transactionIds(2), append.transactionAt(2), transferTx3, recipient.publicKeyHash)

        val reward = 6.waves
        checkBalances(
          append.transactionStateUpdates(0).balances,
          Map(
            (senderAddress, Waves) -> (senderBalanceBefore, senderBalanceBefore - (fee1 + amount)),
            (recipient, Waves)     -> (reward, reward + amount + fee1 / 5 * 2)
          )
        )
        checkBalances(
          append.transactionStateUpdates(1).balances,
          Map(
            (senderAddress, Waves) -> (senderBalanceBefore - (fee1 + amount), senderBalanceBefore - (fee1 + fee2 + 2 * amount)),
            (recipient, Waves)     -> (reward + amount + fee1 / 5 * 2, reward + 2 * amount + (fee1 + fee2) / 5 * 2)
          )
        )
        checkBalances(
          append.transactionStateUpdates(2).balances,
          Map(
            (senderAddress, Waves) -> (senderBalanceBefore - (fee1 + fee2 + 2 * amount), senderBalanceBefore - (fee1 + fee2 + fee3 + 3 * amount)),
            (recipient, Waves)     -> (reward + 2 * amount + (fee1 + fee2) / 5 * 2, reward + 3 * amount + (fee1 + fee2 + fee3) / 5 * 2)
          )
        )
      }
    }

    "BU-9. Return correct data for issue" in {
      val script              = Option(TxHelpers.script("true"))
      val amount: Long        = 599000
      val decimals: Byte      = 8
      val name: String        = "Test_asset"
      val description: String = name + "|_|_|_|_|_|" + 5380000
      val issue               = TxHelpers.issue(sender, amount, decimals, name, description, customAssetIssueFee, script)
      val issueScript         = issue.script.get.bytes.value().arr

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendMicroBlock(issue)) { updates =>
        val append       = updates(1).append
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

    "BU-11. Return correct data for issue NFT" in {
      val name: String        = "Nft_test_asset"
      val description: String = name + "_OVER_9000"
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

      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.ReduceNFTFee),
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendMicroBlock(issueNftTx)) { updates =>
        val append       = updates(1).append
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

    "BU-19. Return correct data for reissue" in {
      val amount: Long               = 9000000
      val amountReissue: Long        = 7500000
      val issue                      = TxHelpers.issue(sender, amount)
      val reissueTx                  = TxHelpers.reissue(issue.asset, sender, amountReissue, reissuable = false, customAssetIssueFee)
      val quantityAfterReissue       = amount + amountReissue
      val senderBalanceBeforeReissue = senderBalanceBefore - issue.fee.value
      val senderBalanceAfterReissue  = senderBalanceBeforeReissue - reissueTx.fee.value

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendMicroBlock(reissueTx)
      } { updates =>
        val append       = updates(2).append
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

    "BU-4. Return correct data for burn" in {
      val amount: Long            = 6000000
      val amountBurn: Long        = 5000000
      val issue                   = TxHelpers.issue(sender, amount)
      val burnTx                  = TxHelpers.burn(issue.asset, amountBurn, sender, customAssetIssueFee)
      val amountAfterTx           = amount - amountBurn
      val senderBalanceBeforeBurn = senderBalanceBefore - issue.fee.value
      val senderBalanceAfterBurn  = senderBalanceBeforeBurn - burnTx.fee.value

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendMicroBlock(burnTx)
      } { updates =>
        val append       = updates(2).append
        val assetDetails = append.transactionStateUpdates.head.assets.head
        checkBurn(append.transactionIds.head, append.transactionAt(0), burnTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (senderAddress, Waves)        -> (senderBalanceBeforeBurn, senderBalanceAfterBurn),
            (senderAddress, burnTx.asset) -> (amount, amountAfterTx)
          )
        )
        checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
        checkAssetsStateUpdates(assetDetails.after, burnTx, isNft = false, amountAfterTx)
      }
    }

    "Exchange transaction subscription tests" - {
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

      "BU-6. Return correct data for order V3, exchange V2" in {
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
        addedBlocksAndSubscribe(exchangeTx) { updated =>
          checkingSubscribeFields(updated(3).getAppend, exchangeTx, exchangedAssets, order1.amount.value)
        }
      }

      "BU-120. Return correct data for order V4, exchange V3" in {
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
        val exchangeTx      = TxHelpers.exchangeFromOrders(order1, order2, buyer, version = TxVersion.V3)
        addedBlocksAndSubscribe(exchangeTx) { updated =>
          checkingSubscribeFields(updated(3).getAppend, exchangeTx, exchangedAssets, order1.amount.value)
        }
      }

      def addedBlocksAndSubscribe(exchangeTx: ExchangeTransaction)(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
        withGenerateSubscription(
          settings = currentSettings,
          balances = Seq(
            AddrWithBalance(buyer.toAddress, buyerBalanceBefore),
            AddrWithBalance(seller.toAddress, sellerBalanceBefore)
          )
        ) { d =>
          d.appendBlock(priceAsset)
          d.appendBlock(amountAsset)
          d.appendMicroBlock(exchangeTx)
        }(f)
      }

      def checkingSubscribeFields(append: Append, exchangeTx: ExchangeTransaction, exchangedAssets: Long, orderAmount: Long): Unit = {
        checkExchange(append.transactionIds.head, append.transactionAt(0), exchangeTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map(
            (buyer.toAddress, Waves)              -> (buyerBalanceBeforeExchange, buyerBalanceBeforeExchange - fee + customFee),
            (seller.toAddress, priceAsset.asset)  -> (0, exchangedAssets),
            (buyer.toAddress, amountAsset.asset)  -> (0, orderAmount),
            (seller.toAddress, Waves)             -> (sellerBalanceBeforeExchange, sellerBalanceBeforeExchange - customFee),
            (buyer.toAddress, priceAsset.asset)   -> (priceAssetQuantity, priceAssetQuantity - exchangedAssets),
            (seller.toAddress, amountAsset.asset) -> (amountAssetQuantity, amountAssetQuantity - orderAmount)
          )
        )
      }
    }

    "BU-12. Return correct data for lease" in {
      val recipient        = TxHelpers.signer(123)
      val recipientAddress = recipient.toAddress
      val amount           = 5.waves
      val lease            = TxHelpers.lease(sender, recipientAddress, amount, customFee)
      val leaseId          = lease.id.value().arr

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendMicroBlock(lease)) { updates =>
        val append = updates(1).append
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
            (senderAddress, 0L, amount)    -> (0L, 0L),
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

    "BU-14. Return correct data for lease cancel" in {
      val recipient             = TxHelpers.signer(123)
      val recipientAddress      = recipient.toAddress
      val amount                = 5.waves
      val lease                 = TxHelpers.lease(sender, recipientAddress, amount, customFee)
      val leaseCancel           = TxHelpers.leaseCancel(lease.id.value(), sender, customFee)
      val leaseId               = leaseCancel.leaseId.arr
      val senderBalanceBeforeTx = senderBalanceBefore - lease.fee.value
      val senderBalanceAfterTx  = senderBalanceBeforeTx - leaseCancel.fee.value

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(lease)
        d.appendMicroBlock(leaseCancel)
      } { updates =>
        val append = updates(2).append
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
            (senderAddress, 0L, 0L)    -> (0L, amount),
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

    "BU-16. Return correct data for massTransfer" in {
      val massTransferFee           = fee * 6
      val transferAmount            = 500000L
      val recipients                = TxHelpers.accountSeqGenerator(100, transferAmount)
      val issue                     = TxHelpers.issue(sender, 1000000000L)
      val issuedAsset: Asset        = Asset.fromCompatId(issue.asset.compatId)
      val massTransfer              = TxHelpers.massTransfer(sender, recipients, issuedAsset, massTransferFee)
      val senderBalanceBeforeTx     = senderBalanceBefore - issue.fee.value
      val senderBalanceAfterTx      = senderBalanceBeforeTx - massTransfer.fee.value
      val senderAssetBalanceAfterTx = issue.quantity.value - transferAmount * recipients.size

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendMicroBlock(massTransfer)
      } { updates =>
        val balancesMap = Map(
          (senderAddress, Waves)       -> (senderBalanceBeforeTx, senderBalanceAfterTx),
          (senderAddress, issuedAsset) -> (issue.quantity.value, senderAssetBalanceAfterTx)
        ) ++ recipients.map(r => (Address.fromBytes(r.address.bytes).explicitGet(), issuedAsset) -> (0L, transferAmount)).toMap
        val append = updates(2).append
        checkMassTransfer(
          append.transactionIds.head,
          append.transactionAt(0),
          massTransfer,
          recipients.map(r =>
            r.address match {
              case address: Address => Some(address.publicKeyHash).get
              case _                => fail("not an address")
            }
          )
        )
        checkMassTransferBalances(append.transactionStateUpdates.head.balances, balancesMap)
      }
    }

    "BU-5. Return correct data for data" in {
      val integerDataEntry = IntegerDataEntry.apply("Integer", 3550000L)
      val booleanDataEntry = BooleanDataEntry.apply("Boolean", value = true)
      val stringDataEntry  = StringDataEntry.apply("String", "test")
      val binaryDataEntry  = BinaryDataEntry.apply("Binary", ByteStr.apply(senderAddress.bytes))
      val entries          = Seq(booleanDataEntry, integerDataEntry, stringDataEntry, binaryDataEntry)
      val dataTx           = TxHelpers.data(sender, entries, customFee, TxVersion.V2)

      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendMicroBlock(dataTx)) { updates =>
        val append    = updates(1).append
        val txUpdates = append.transactionStateUpdates.head
        checkDataTransaction(append.transactionIds.head, append.transactionAt(0), dataTx)
        checkBalances(
          txUpdates.balances,
          Map((senderAddress, Waves) -> (senderBalanceBefore, senderBalanceBefore - customFee))
        )
        checkDataEntriesStateUpdate(txUpdates.dataEntries, dataTx.data, senderAddress.bytes)
      }
    }

    "BU-21. Return correct data for setScript" in {
      val setScript = TxHelpers.setScript(sender, testScript, customFee)

      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendMicroBlock(setScript)) { updates =>
        val append    = updates(1).append
        val txUpdates = append.transactionStateUpdates.head
        checkSetScriptTransaction(append.transactionIds.head, append.transactionAt(0), setScript)
        checkBalances(
          txUpdates.balances,
          Map((senderAddress, Waves) -> (senderBalanceBefore, senderBalanceBefore - customFee))
        )
        checkSetScriptStateUpdate(txUpdates.scripts.head, setScript)
      }
    }

    "Return correct data for sponsorFee" - {
      val sponsorshipFee                       = Option.apply(3950000L)
      val issue                                = TxHelpers.issue(sender, 99900000L)
      val sponsorFee                           = TxHelpers.sponsor(issue.asset, sponsorshipFee, sender)
      val sponsorFeeCancel                     = TxHelpers.sponsor(issue.asset, None, sender)
      val senderBalanceBeforeSponsorFeeTx      = senderBalanceBefore - issue.fee.value
      val senderBalanceAfterSponsorFeeTx       = senderBalanceBeforeSponsorFeeTx - sponsorFee.fee.value
      val senderBalanceAfterSponsorFeeCancelTx = senderBalanceAfterSponsorFeeTx - sponsorFeeCancel.fee.value

      "BU-25. subscribe sponsorFee " in withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(sponsorFee)
      } { updates =>
        val append       = updates(2).append
        val txUpdates    = append.transactionStateUpdates.head
        val assetDetails = txUpdates.assets.head

        checkSponsorFeeTransaction(append.transactionIds.head, append.transactionAt(0), sponsorFee)
        checkBalances(
          txUpdates.balances,
          Map((senderAddress, Waves) -> (senderBalanceBeforeSponsorFeeTx, senderBalanceAfterSponsorFeeTx))
        )
        checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
        checkAssetsStateUpdates(assetDetails.after, issue, isNft = false, issue.quantity.value)
      }

      "BU-27. subscribe sponsorFee cancel" in withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(sponsorFee)
        d.appendMicroBlock(sponsorFeeCancel)
      } { updates =>
        val append       = updates(3).append
        val txUpdates    = append.transactionStateUpdates.head
        val assetDetails = txUpdates.assets.head

        checkSponsorFeeTransaction(append.transactionIds.head, append.transactionAt(0), sponsorFeeCancel)
        checkBalances(
          txUpdates.balances,
          Map((senderAddress, Waves) -> (senderBalanceAfterSponsorFeeTx, senderBalanceAfterSponsorFeeCancelTx))
        )
        checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
        checkAssetsStateUpdates(assetDetails.after, issue, isNft = false, issue.quantity.value)
      }
    }

    "BU-20. Return correct data for setAssetScript" in {
      val complexScriptBefore                 = Option.apply(TxHelpers.script("true".stripMargin))
      val complexScriptAfter                  = TxHelpers.script("false".stripMargin)
      val issue                               = TxHelpers.issue(sender, 99900000L, script = complexScriptBefore)
      val setAssetScript                      = TxHelpers.setAssetScript(sender, issue.asset, complexScriptAfter, 1.waves)
      val quantity                            = issue.quantity.value
      val senderBalanceBeforeSetAssetScriptTx = senderBalanceBefore - issue.fee.value
      val senderBalanceAfterSetAssetScriptTx  = senderBalanceBeforeSetAssetScriptTx - setAssetScript.fee.value

      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendMicroBlock(setAssetScript)
      } { updates =>
        val append       = updates(2).append
        val txUpdates    = append.transactionStateUpdates.head
        val assetDetails = txUpdates.assets.head
        checkSetAssetScriptTransaction(append.transactionIds.head, append.transactionAt(0), setAssetScript)
        checkBalances(
          txUpdates.balances,
          Map(
            (senderAddress, Waves) -> (senderBalanceBeforeSetAssetScriptTx, senderBalanceAfterSetAssetScriptTx)
          )
        )
        checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, quantity)
        checkAssetsScriptStateUpdates(assetDetails.before.get.scriptInfo, complexScriptBefore.get.bytes.value().arr)
        checkAssetsStateUpdates(assetDetails.after, issue, isNft = false, quantity)
        checkAssetsScriptStateUpdates(assetDetails.after.get.scriptInfo, complexScriptAfter.bytes.value().arr)
      }
    }

    "BU-121. Return correct data for UpdateAssetInfo" in {
      val newName                              = "new_name"
      val newDescription                       = "new_description"
      val issue                                = TxHelpers.issue(sender, 99900000L)
      val updateAssetInfo                      = TxHelpers.updateAssetInfo(issue.assetId, newName, newDescription, sender)
      val senderBalanceBeforeUpdateAssetInfoTx = senderBalanceBefore - issue.fee.value
      val senderBalanceAfterUpdateAssetInfoTx  = senderBalanceBeforeUpdateAssetInfoTx - updateAssetInfo.fee

      withGenerateSubscription(
        settings = currentSettings.configure(_.copy(minAssetInfoUpdateInterval = 1)),
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock()
        d.appendMicroBlock(updateAssetInfo)
      } { updates =>
        val append       = updates(3).append
        val txUpdates    = append.transactionStateUpdates.head
        val assetDetails = txUpdates.assets.head
        checkUpdateAssetInfoTransaction(append.transactionIds.head, append.transactionAt(0), updateAssetInfo)
        checkBalances(
          txUpdates.balances,
          Map(
            (senderAddress, Waves) -> (senderBalanceBeforeUpdateAssetInfoTx, senderBalanceAfterUpdateAssetInfoTx)
          )
        )
        checkAssetsStateUpdates(assetDetails.before, issue, isNft = false, issue.quantity.value)
        checkAssetUpdatesStateUpdates(assetDetails.after, updateAssetInfo)
      }
    }
  }
}
