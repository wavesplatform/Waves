package com.wavesplatform.events

import com.wavesplatform.TestValues.fee
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.{EthTxGenerator, EthereumTransaction, TxHelpers, TxVersion}
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated

class BlockchainUpdatesSubscribeSpec extends BlockchainUpdatesTestBase {
  "BlockchainUpdates subscribe tests" - {
    "BU-1. Return correct data for alias" in {
      val aliasTx = TxHelpers.createAlias("test", firstTxParticipant, fee = customFee)
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendMicroBlock(aliasTx)) { updates =>
        val append = updates(1).append
        checkAlias(append, aliasTx)
      }
    }

    "BU-28. Return correct data for transfer" in {
      val transferTx = TxHelpers.transfer(firstTxParticipant, secondTxParticipantAddress, amount, Waves, customFee)
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondTxParticipant.toAddress, secondTxParticipantBalanceBefore)
        )
      )(_.appendMicroBlock(transferTx)) { updates =>
        val append = updates(1).append
        checkTransferTx(append, transferTx)
      }
    }

    "BU-9. Return correct data for issue" in {
      val issue: IssueTransaction = TxHelpers.issue(
        firstTxParticipant,
        amount,
        decimals = 8,
        name = "Test_asset",
        description = "description",
        customAssetIssueFee,
        defaultScript
      )
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendMicroBlock(issue)) { updates =>
        val append = updates(1).append
        checkIssueTx(append, issue, isNft = false)
      }
    }

    "BU-11. Return correct data for issue NFT" in {
      val issueNftTx =
        TxHelpers.issue(firstTxParticipant, name = "Nft_test_asset", description = "OVER_9000", amount = 1, reissuable = false, script = None)
      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.ReduceNFTFee),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendMicroBlock(issueNftTx)) { updates =>
        val append = updates(1).append
        checkIssueTx(append, issueNftTx, isNft = true)
      }
    }

    "BU-19. Return correct data for reissue" in {
      val issue   = TxHelpers.issue(firstTxParticipant, amount)
      val reissue = TxHelpers.reissue(issue.asset, firstTxParticipant, additionalAmount, reissuable = false, customAssetIssueFee)
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(reissue)
      } { updates =>
        val append = updates(2).append
        checkReissueTx(append, reissue, issue)
      }
    }

    "BU-4. Return correct data for burn" in {
      val issue = TxHelpers.issue(firstTxParticipant, amount)
      val burn  = TxHelpers.burn(issue.asset, additionalAmount, firstTxParticipant, customAssetIssueFee)
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(burn)
      } { updates =>
        val append = updates(2).append
        checkBurnTx(append, burn, issue)
      }
    }

    "Exchange transaction subscription tests" - {
      "BU-6. Return correct data for order V3, exchange V2" in {
        val order1          = createOrder(OrderType.BUY, firstTxParticipant, Order.V3)
        val order2          = createOrder(OrderType.SELL, secondTxParticipant, Order.V3)
        val normalizedPrice = order1.price.value * order1.amount.value / 100000000
        val exchangeTx      = TxHelpers.exchangeFromOrders(order1, order2, firstTxParticipant, version = TxVersion.V2)
        withAddedBlocksAndSubscribeExchangeTx(exchangeTx) { updated =>
          val append = updated.apply(3).getAppend
          checkExchangeTx(append, exchangeTx, normalizedPrice, order1.amount.value)
        }
      }

      "BU-120. Return correct data for order V4, exchange V3" in {
        val order1          = createOrder(OrderType.BUY, firstTxParticipant, Order.V4)
        val order2          = createOrder(OrderType.SELL, secondTxParticipant, Order.V4)
        val normalizedPrice = order1.price.value / 2 / 10000000
        val exchangeTx      = TxHelpers.exchangeFromOrders(order1, order2, firstTxParticipant, version = TxVersion.V3)
        withAddedBlocksAndSubscribeExchangeTx(exchangeTx) { updated =>
          val append = updated.apply(3).getAppend
          checkExchangeTx(append, exchangeTx, normalizedPrice, order1.amount.value)
        }
      }
    }

    "BU-12. Return correct data for lease" in {
      val lease = TxHelpers.lease(firstTxParticipant, secondTxParticipantAddress, amount, customFee)
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendMicroBlock(lease)) { updates =>
        val append = updates(1).append
        checkLeaseTx(append, lease)
      }
    }

    "BU-14. Return correct data for lease cancel" in {
      val lease       = TxHelpers.lease(firstTxParticipant, secondTxParticipantAddress, amount, customFee)
      val leaseCancel = TxHelpers.leaseCancel(lease.id.value(), firstTxParticipant, customFee)
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(lease)
        d.appendMicroBlock(leaseCancel)
      } { updates =>
        val append = updates(2).append
        checkLeaseCancelTx(append, leaseCancel, lease)
      }
    }

    "BU-16. Return correct data for massTransfer" in {
      val massTransferFee = fee * 6
      val massTransfer    = TxHelpers.massTransfer(firstTxParticipant, recipients, firstToken.asset, massTransferFee)

      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendMicroBlock(massTransfer)
      } { updates =>
        val append = updates(2).append
        checkForMassTransferTx(append, massTransfer)
      }
    }

    "BU-5. Return correct data for dataTx" in {
      val data = TxHelpers.data(firstTxParticipant, entries, customFee, TxVersion.V2)
      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendMicroBlock(data)) { updates =>
        val append = updates(1).append
        checkDataTransfer(append, data)
      }
    }

    "BU-21. Return correct data for setScript" in {
      val setScript = TxHelpers.setScript(firstTxParticipant, testScript, customFee)

      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendMicroBlock(setScript)) { updates =>
        val append = updates(1).append
        checkSetScript(append, setScript)
      }
    }

    "Return correct data for sponsorFee" - {
      val sponsorshipFee                       = Option.apply(additionalAmount)
      val sponsorFee                           = TxHelpers.sponsor(firstToken.asset, sponsorshipFee, firstTxParticipant)
      val sponsorFeeCancel                     = TxHelpers.sponsor(firstToken.asset, None, firstTxParticipant)
      val senderBalanceBeforeSponsorFeeTx      = firstTxParticipantBalanceBefore - firstToken.fee.value
      val senderBalanceAfterSponsorFeeTx       = senderBalanceBeforeSponsorFeeTx - sponsorFee.fee.value
      val senderBalanceAfterSponsorFeeCancelTx = senderBalanceAfterSponsorFeeTx - sponsorFee.fee.value

      "BU-25. subscribe sponsorFee" in withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(sponsorFee)
      } { updates =>
        val append = updates(2).append
        checkSponsorFee(append, sponsorFee, senderBalanceBeforeSponsorFeeTx, senderBalanceAfterSponsorFeeTx)
      }

      "BU-27. subscribe sponsorFee cancel" in withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(sponsorFee)
        d.appendMicroBlock(sponsorFeeCancel)
      } { updates =>
        val append = updates(3).append
        checkSponsorFee(append, sponsorFeeCancel, senderBalanceAfterSponsorFeeTx, senderBalanceAfterSponsorFeeCancelTx)
      }
    }

    "BU-20. Return correct data for setAssetScript" in {
      val issue          = TxHelpers.issue(firstTxParticipant, amount, script = complexScriptBefore)
      val setAssetScript = TxHelpers.setAssetScript(firstTxParticipant, issue.asset, complexScriptAfter, 1.waves)
      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendMicroBlock(setAssetScript)
      } { updates =>
        val append = updates(2).append
        checkSetAssetScript(append, setAssetScript, issue)
      }
    }

    "BU-121. Return correct data for UpdateAssetInfo" in {
      val newName         = "new_name"
      val newDescription  = "new_description"
      val updateAssetInfo = TxHelpers.updateAssetInfo(firstTokenAsset.id, newName, newDescription, firstTxParticipant)

      withGenerateSubscription(
        settings = currentSettings.configure(_.copy(minAssetInfoUpdateInterval = 1)),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock()
        d.appendMicroBlock(updateAssetInfo)
      } { updates =>
        val append = updates(3).append
        checkUpdateAssetInfo(append, updateAssetInfo)
      }
    }

    "BU-122. Return correct data for EthereumTransfer" in {
      val ethereumTransfer: EthereumTransaction =
        EthTxGenerator.generateEthTransfer(firstTxParticipantEthereum, secondTxParticipantAddress, amount, secondTokenAsset)
      val ethAddress = ethereumTransfer.senderAddress.value()
      val transfer   = TxHelpers.transfer(secondTxParticipant, ethAddress, secondTokenQuantity, secondTokenAsset)

      withGenerateSubscription(
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(
          AddrWithBalance(ethAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
        )
      ) { d =>
        d.appendKeyBlock()
        d.appendBlock(secondToken, transfer)
        d.appendMicroBlock(ethereumTransfer)
      } { updates =>
        val append = updates(3).append
        checkEthereumTransfer(append, ethereumTransfer, ethAddress)
      }
    }

    def withAddedBlocksAndSubscribeExchangeTx(exchangeTx: ExchangeTransaction)(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
        )
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(secondToken)
        d.appendMicroBlock(exchangeTx)
      }(f)
    }
  }
}
