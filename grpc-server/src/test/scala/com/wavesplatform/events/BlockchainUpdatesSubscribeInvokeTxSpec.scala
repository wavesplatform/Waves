package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.{dataMap, invokeAssetScript, issueAssetAmount, scriptTransferIssueAssetInt}
import com.wavesplatform.events.fixtures.WavesTxChecks.{
  checkAssetsStateUpdates,
  checkBalances,
  checkDataEntriesStateUpdate,
  checkIndividualLeases,
  checkInvokeBaseTransactionMetadata,
  checkInvokeResultTransactionMetadata,
  checkInvokeTransaction
}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, EXPR}
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.events.protobuf.BlockchainUpdated

import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, StringDataEntry}
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeInvokeTxSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6
  val dAppAccount: SeedKeyPair       = TxHelpers.signer(12)
  val dAppAddress: Address           = dAppAccount.toAddress
  var dAppBalanceBefore: Long        = 10.waves
  val sender: SeedKeyPair            = TxHelpers.signer(2436)
  val senderAddress: Address         = sender.toAddress
  val senderBalanceBefore: Long      = 10.waves
  var dAppBalanceAfterTx             = 0L
  var dAppAssetBalanceAfterTx: Long  = 0L
  var senderBalanceAfterTx           = 0L

  "BU-31. Return correct data for Invoke" in {
    val issue                        = TxHelpers.issue(dAppAccount)
    val asset                        = issue.asset
    val assetByteStr                 = CONST_BYTESTR(asset.id).explicitGet()
    val addressByteStr               = CONST_BYTESTR(ByteStr.apply(senderAddress.bytes)).explicitGet()
    val args: Seq[EXPR]              = Seq(assetByteStr, addressByteStr)
    val invoke                       = TxHelpers.invoke(dAppAddress, Some("setData"), args, Seq.empty, sender, fee = 100500000L)
    val dAppInvokeIssueBalance: Long = issueAssetAmount - scriptTransferIssueAssetInt
    val scriptTransferAssetInt: Long = dataMap.apply("scriptTransferAssetInt").toString.toLong
    val dataEntry: Seq[DataEntry[?]] = Seq[DataEntry[?]](
      EmptyDataEntry("int"),
      BinaryDataEntry("byte", asset.id),
      BooleanDataEntry("bool", value = true),
      StringDataEntry("str", "test_string")
    )

    withGenerateSubscription(
      settings = currentSettings,
      balances = Seq(
        AddrWithBalance(dAppAddress, dAppBalanceBefore),
        AddrWithBalance(senderAddress, senderBalanceBefore)
      )
    ) { d =>
      d.appendBlock(TxHelpers.setScript(dAppAccount, TxHelpers.script(invokeAssetScript)))
      d.appendBlock(issue)
      dAppBalanceBefore = d.balance(dAppAddress)
      d.appendMicroBlock(invoke)
      senderBalanceAfterTx = d.balance(senderAddress)
      dAppAssetBalanceAfterTx = d.balance(dAppAddress, asset)
      dAppBalanceAfterTx = d.balance(dAppAddress)
    } { updates =>
      checkInvokeBlockchainUpdates(updates)
    }

    def checkInvokeBlockchainUpdates(updates: Seq[BlockchainUpdated]): Unit = {
      val append = updates(3).append
      val transactionMetadata = append.transactionsMetadata
      val result = transactionMetadata.head.getInvokeScript.result
      val dataEntries = append.transactionStateUpdates.head.dataEntries
      val invokeIssueAsset = toVanillaAssetId(result.get.issues.head.assetId)
      val invokeLeaseId = result.get.leases.head.leaseId.toByteArray
      val assetDetails = append.transactionStateUpdates.head.assets

      checkInvokeTransaction(append.transactionIds.head, append.transactionAt(0), invoke, dAppAddress.publicKeyHash)
      checkInvokeBaseTransactionMetadata(transactionMetadata, invoke, asset.id, senderAddress.bytes)
      checkInvokeResultTransactionMetadata(result, dataMap, asset.id, senderAddress)
      checkBalances(
        append.transactionStateUpdates.head.balances,
        Map(
          (senderAddress, Waves) -> (senderBalanceBefore, senderBalanceAfterTx),
          (senderAddress, asset) -> (0, scriptTransferAssetInt),
          (senderAddress, invokeIssueAsset) -> (0, dataMap.apply("scriptTransferIssueAssetInt").toString.toLong),
          (dAppAddress, Waves) -> (dAppBalanceBefore, dAppBalanceAfterTx),
          (dAppAddress, invokeIssueAsset) -> (0, dAppInvokeIssueBalance),
          (dAppAddress, asset) -> (issue.quantity.value, dAppAssetBalanceAfterTx)
        )
      )
      checkDataEntriesStateUpdate(dataEntries, dataEntry, dAppAddress.bytes)
      checkAssetsStateUpdates(assetDetails.head.after, dataMap, invokeIssueAsset, dAppAccount.publicKey.arr)
      checkAssetsStateUpdates(assetDetails.apply(1).before, issue, isNft = false, issue.quantity.value)
      checkAssetsStateUpdates(assetDetails.apply(1).after, issue, isNft = false, dAppAssetBalanceAfterTx + scriptTransferAssetInt)
      checkIndividualLeases(
        append.transactionStateUpdates.head.individualLeases,
        Map(
          (
            LeaseStatus.Inactive,
            dataMap.apply("leaseInt").toString.toLong
          ) -> (invokeLeaseId, dAppAccount.publicKey.arr, senderAddress.bytes, invoke.id.value().arr)
        )
      )
    }
  }
}
