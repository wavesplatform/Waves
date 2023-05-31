package com.wavesplatform.events

import com.wavesplatform.TestValues.fee
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.api.grpc.protobuf.GetBlockUpdatesRangeRequest
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.transaction.{TxHelpers, TxVersion}

class BlockchainUpdatesGetBlockUpdatesRangeSpec extends BlockchainUpdatesTestBase {
  "BlockchainUpdates getBlockUpdateRange tests" - {
    "BU- . Return correct data for alias" in {
      val aliasTx = TxHelpers.createAlias("test", firstTxParticipant, fee = customFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 2),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(aliasTx)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(1).getAppend
        checkingAlias(append, aliasTx)
      }
    }

    "BU- . Return correct data for transfer" in {
      val transferTx = TxHelpers.transfer(firstTxParticipant, secondTxParticipantAddress, amount, Waves, customFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 2),
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondTxParticipant.toAddress, secondTxParticipantBalanceBefore)
        )
      ) { d =>
        d.appendBlock(transferTx)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(1).getAppend
        checkingTransferTx(append, transferTx)
      }
    }

    "BU- . Return correct data for issue" in {
      val issue: IssueTransaction = TxHelpers.issue(
        firstTxParticipant,
        amount,
        decimals = 8,
        name = "Test_asset",
        description = "description",
        customAssetIssueFee,
        defaultScript
      )
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 2),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(1).getAppend
        checkingIssueTx(append, issue, isNft = false)
      }
    }

    "BU- . Return correct data for issue NFT" in {
      val issueNftTx =
        TxHelpers.issue(firstTxParticipant, name = "Nft_test_asset", description = "OVER_9000", amount = 1, reissuable = false, script = None)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 2),
        settings = currentSettings.addFeatures(BlockchainFeatures.ReduceNFTFee),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issueNftTx)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(1).getAppend
        checkingIssueTx(append, issueNftTx, isNft = true)
      }
    }

    "BU- . Return correct data for reissue" in {
      val issue   = TxHelpers.issue(firstTxParticipant, amount)
      val reissue = TxHelpers.reissue(issue.asset, firstTxParticipant, additionalAmount, reissuable = false, customAssetIssueFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 3),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(reissue)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(2).getAppend
        checkingReissueTx(append, reissue, issue)
      }
    }

    "BU- . Return correct data for burn" in {
      val issue = TxHelpers.issue(firstTxParticipant, amount)
      val burn  = TxHelpers.burn(issue.asset, additionalAmount, firstTxParticipant, customAssetIssueFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 3),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(burn)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(2).getAppend
        checkingBurnTx(append, burn, issue)
      }
    }

    "Exchange transaction subscription tests" - {
      "BU- . Return correct data for order V3, exchange V2" in {
        val order1          = createOrders(OrderType.BUY, firstTxParticipant, Order.V3)
        val order2          = createOrders(OrderType.SELL, secondTxParticipant, Order.V3)
        val exchangedAssets = order1.price.value * order1.amount.value / 100000000
        val exchangeTx      = TxHelpers.exchangeFromOrders(order1, order2, firstTxParticipant, version = TxVersion.V2)
        addedBlocksAndGetBlockUpdate(exchangeTx, height = 4) { getBlockUpdate =>
          val append = getBlockUpdate.getUpdate.getAppend
          checkingExchangeTx(append, exchangeTx, exchangedAssets, order1.amount.value)
        }
      }

      "BU- . Return correct data for order V4, exchange V3" in {
        val order1          = createOrders(OrderType.BUY, firstTxParticipant, Order.V4)
        val order2          = createOrders(OrderType.SELL, secondTxParticipant, Order.V4)
        val normalizedPrice = order1.price.value / 2 / 10000000
        val exchangeTx      = TxHelpers.exchangeFromOrders(order1, order2, firstTxParticipant, version = TxVersion.V3)
        addedBlocksAndGetBlockUpdate(exchangeTx, height = 4) { getBlockUpdate =>
          val append = getBlockUpdate.getUpdate.getAppend
          checkingExchangeTx(append, exchangeTx, normalizedPrice, order1.amount.value)
        }
      }
    }

    "BU- . Return correct data for lease" in {
      val lease = TxHelpers.lease(firstTxParticipant, secondTxParticipantAddress, amount, customFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 2),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(lease)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(1).getAppend
        checkingLeaseTx(append, lease)
      }
    }

    "BU- . Return correct data for lease cancel" in {
      val lease       = TxHelpers.lease(firstTxParticipant, secondTxParticipantAddress, amount, customFee)
      val leaseCancel = TxHelpers.leaseCancel(lease.id.value(), firstTxParticipant, customFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 3),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(lease)
        d.appendBlock(leaseCancel)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(2).getAppend
        checkingLeaseCancelTx(append, leaseCancel, lease)
      }
    }

    "BU- . Return correct data for massTransfer" in {
      val massTransferFee = fee * 6
      val massTransfer    = TxHelpers.massTransfer(firstTxParticipant, recipients, firstToken.asset, massTransferFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 3),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(massTransfer)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(2).getAppend
        checkingMassTransfer(append, massTransfer)
      }
    }

    "BU- . Return correct data for data" in {
      val data = TxHelpers.data(firstTxParticipant, entries, customFee, TxVersion.V2)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 2),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(data)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(1).getAppend
        checkingDataTransfer(append, data)
      }
    }

    "BU- . Return correct data for setScript" in {
      val setScript = TxHelpers.setScript(firstTxParticipant, testScript, customFee)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 2),
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(setScript)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(1).getAppend
        checkingSetScript(append, setScript)
      }
    }

    "Return correct data for sponsorFee" - {
      val sponsorshipFee                       = Option.apply(additionalAmount)
      val sponsorFee                           = TxHelpers.sponsor(firstToken.asset, sponsorshipFee, firstTxParticipant)
      val sponsorFeeCancel                     = TxHelpers.sponsor(firstToken.asset, None, firstTxParticipant)
      val senderBalanceBeforeSponsorFeeTx      = firstTxParticipantBalanceBefore - firstToken.fee.value
      val senderBalanceAfterSponsorFeeTx       = senderBalanceBeforeSponsorFeeTx - sponsorFee.fee.value
      val senderBalanceAfterSponsorFeeCancelTx = senderBalanceAfterSponsorFeeTx - sponsorFee.fee.value

      "BU-  sponsorFee" in withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 3),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(sponsorFee)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(2).getAppend
        checkingSponsorFee(append, sponsorFee, senderBalanceBeforeSponsorFeeTx, senderBalanceAfterSponsorFeeTx)
      }

      "BU-  sponsorFee cancel" in withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 4),
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(sponsorFee)
        d.appendBlock(sponsorFeeCancel)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(3).getAppend
        checkingSponsorFee(append, sponsorFeeCancel, senderBalanceAfterSponsorFeeTx, senderBalanceAfterSponsorFeeCancelTx)
      }
    }

    "BU- . Return correct data for setAssetScript" in {
      val issue          = TxHelpers.issue(firstTxParticipant, amount, script = complexScriptBefore)
      val setAssetScript = TxHelpers.setAssetScript(firstTxParticipant, issue.asset, complexScriptAfter, 1.waves)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 3),
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(setAssetScript)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(2).getAppend
        checkingSetAssetScript(append, setAssetScript, issue)
      }
    }

    "BU- . Return correct data for UpdateAssetInfo" in {
      val newName         = "new_name"
      val newDescription  = "new_description"
      val updateAssetInfo = TxHelpers.updateAssetInfo(firstTokenAsset.id, newName, newDescription, firstTxParticipant)
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 4),
        settings = currentSettings.configure(_.copy(minAssetInfoUpdateInterval = 1)),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock()
        d.appendBlock(updateAssetInfo)
        d.appendBlock()
      } { getBlockUpdateRange =>
        val append = getBlockUpdateRange.apply(3).getAppend
        checkingUpdateAssetInfo(append, updateAssetInfo)
      }
    }
  }
}
