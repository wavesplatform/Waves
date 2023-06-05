package com.wavesplatform.events

import com.wavesplatform.TestValues.fee
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.transaction.{EthTxGenerator, EthereumTransaction, TxHelpers, TxVersion}

class BlockchainUpdatesGetBlockUpdatesSpec extends BlockchainUpdatesTestBase {
  "BlockchainUpdates getBlockUpdate tests" - {
    "BU-192. Return correct data for alias from getBlockUpdate" in {
      val aliasTx = TxHelpers.createAlias("test", firstTxParticipant, fee = customFee)
      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendBlock(aliasTx)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingAlias(append, aliasTx)
      }
    }

    "BU-207. Return correct data for transfer" in {
      val transferTx = TxHelpers.transfer(firstTxParticipant, secondTxParticipantAddress, amount, Waves, customFee)
      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondTxParticipant.toAddress, secondTxParticipantBalanceBefore)
        )
      )(_.appendBlock(transferTx)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingTransferTx(append, transferTx)
      }
    }

    "BU-196. Return correct data for issue" in {
      val issue: IssueTransaction = TxHelpers.issue(
        firstTxParticipant,
        amount,
        decimals = 8,
        name = "Test_asset",
        description = "description",
        customAssetIssueFee,
        defaultScript
      )

      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendBlock(issue)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingIssueTx(append, issue, isNft = false)
      }
    }

    "BU-197. Return correct data for issue NFT" in {
      val issueNftTx =
        TxHelpers.issue(firstTxParticipant, name = "Nft_test_asset", description = "OVER_9000", amount = 1, reissuable = false, script = None)
      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings.addFeatures(BlockchainFeatures.ReduceNFTFee),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendBlock(issueNftTx)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingIssueTx(append, issueNftTx, isNft = true)
      }
    }

    "BU-201. Return correct data for reissue" in {
      val issue   = TxHelpers.issue(firstTxParticipant, amount)
      val reissue = TxHelpers.reissue(issue.asset, firstTxParticipant, additionalAmount, reissuable = false, customAssetIssueFee)
      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(reissue)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingReissueTx(append, reissue, issue)
      }
    }

    "BU-193. Return correct data for burn" in {
      val issue = TxHelpers.issue(firstTxParticipant, amount)
      val burn  = TxHelpers.burn(issue.asset, additionalAmount, firstTxParticipant, customAssetIssueFee)
      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(burn)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingBurnTx(append, burn, issue)
      }
    }

    "Exchange transaction subscription tests" - {
      "BU-195. Return correct data for order V3, exchange V2" in {
        val order1          = createOrders(OrderType.BUY, firstTxParticipant, Order.V3)
        val order2          = createOrders(OrderType.SELL, secondTxParticipant, Order.V3)
        val normalizedPrice = order1.price.value * order1.amount.value / 100000000
        val exchangeTx      = TxHelpers.exchangeFromOrders(order1, order2, firstTxParticipant, version = TxVersion.V2)
        addedBlocksAndGetBlockUpdate(exchangeTx, height = 4) { getBlockUpdate =>
          val append = getBlockUpdate.getUpdate.getAppend
          checkingExchangeTx(append, exchangeTx, normalizedPrice, order1.amount.value)
        }
      }

      "BU-223. Return correct data for order V4, exchange V3" in {
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

    "BU-198. Return correct data for lease" in {
      val lease = TxHelpers.lease(firstTxParticipant, secondTxParticipantAddress, amount, customFee)
      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendBlock(lease)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingLeaseTx(append, lease)
      }
    }

    "BU-199. Return correct data for lease cancel" in {
      val lease       = TxHelpers.lease(firstTxParticipant, secondTxParticipantAddress, amount, customFee)
      val leaseCancel = TxHelpers.leaseCancel(lease.id.value(), firstTxParticipant, customFee)
      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(lease)
        d.appendBlock(leaseCancel)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingLeaseCancelTx(append, leaseCancel, lease)
      }
    }

    "BU-200. Return correct data for massTransfer" in {
      val massTransferFee = fee * 6
      val massTransfer    = TxHelpers.massTransfer(firstTxParticipant, recipients, firstToken.asset, massTransferFee)

      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(massTransfer)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingMassTransfer(append, massTransfer)
      }
    }

    "BU-194. Return correct data for dataTx" in {
      val data = TxHelpers.data(firstTxParticipant, entries, customFee, TxVersion.V2)
      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendBlock(data)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingDataTransfer(append, data)
      }
    }

    "BU-203. Return correct data for setScript" in {
      val setScript = TxHelpers.setScript(firstTxParticipant, testScript, customFee)
      withGenerateGetBlockUpdate(
        height = 2,
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      )(_.appendBlock(setScript)) { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
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

      "BU-204. getBlockUpdate sponsorFee" in withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(sponsorFee)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingSponsorFee(append, sponsorFee, senderBalanceBeforeSponsorFeeTx, senderBalanceAfterSponsorFeeTx)
      }

      "BU-206. getBlockUpdate sponsorFee cancel" in withGenerateGetBlockUpdate(
        height = 4,
        settings = currentSettings,
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock(sponsorFee)
        d.appendBlock(sponsorFeeCancel)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingSponsorFee(append, sponsorFeeCancel, senderBalanceAfterSponsorFeeTx, senderBalanceAfterSponsorFeeCancelTx)
      }
    }

    "BU-202. Return correct data for setAssetScript" in {
      val issue          = TxHelpers.issue(firstTxParticipant, amount, script = complexScriptBefore)
      val setAssetScript = TxHelpers.setAssetScript(firstTxParticipant, issue.asset, complexScriptAfter, 1.waves)
      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(issue)
        d.appendBlock(setAssetScript)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingSetAssetScript(append, setAssetScript, issue)
      }
    }

    "BU-224. Return correct data for UpdateAssetInfo" in {
      val newName         = "new_name"
      val newDescription  = "new_description"
      val updateAssetInfo = TxHelpers.updateAssetInfo(firstTokenAsset.id, newName, newDescription, firstTxParticipant)
      withGenerateGetBlockUpdate(
        height = 4,
        settings = currentSettings.configure(_.copy(minAssetInfoUpdateInterval = 1)),
        balances = Seq(AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore))
      ) { d =>
        d.appendBlock(firstToken)
        d.appendBlock()
        d.appendBlock(updateAssetInfo)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingUpdateAssetInfo(append, updateAssetInfo)
      }
    }

    "BU-. Return correct data for EthereumTransfer" in {
      val ethereumTransfer: EthereumTransaction =
        EthTxGenerator.generateEthTransfer(firstTxParticipantEthereum, secondTxParticipantAddress, amount, secondTokenAsset)
      val ethAddress = ethereumTransfer.senderAddress.value()
      val transfer = TxHelpers.transfer(secondTxParticipant, ethAddress, secondTokenQuantity, secondTokenAsset)

      withGenerateGetBlockUpdate(
        height = 4,
        settings = currentSettings.addFeatures(BlockchainFeatures.SmartAccounts),
        balances = Seq(
          AddrWithBalance(ethAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
        )
      ) { d =>
        d.appendKeyBlock()
        d.appendBlock(secondToken, transfer)
        d.appendBlock(ethereumTransfer)
      } { getBlockUpdate =>
        val append = getBlockUpdate.getUpdate.getAppend
        checkingEthereumTransfer(append, ethereumTransfer, ethAddress)
      }
    }
  }
}
