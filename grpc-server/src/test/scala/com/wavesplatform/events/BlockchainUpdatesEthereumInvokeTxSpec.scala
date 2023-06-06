package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdatesRangeRequest, SubscribeRequest}
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.*
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, CONST_STRING, EXPR}
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.EthTxGenerator.Arg
import com.wavesplatform.transaction.{EthTxGenerator, EthereumTransaction, TxHelpers, TxNonNegativeAmount}
import com.wavesplatform.transaction.TxHelpers.{secondAddress, secondSigner}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer

class BlockchainUpdatesEthereumInvokeTxSpec extends BlockchainUpdatesTestBase {
  "Simple invoke transaction" - {
    val issue          = TxHelpers.issue(secondTxParticipant)
    val asset          = issue.asset
    val assetByteStr   = Arg.Bytes(asset.id)
    val addressByteStr = Arg.Bytes(ByteStr.apply(secondTxParticipantAddress.bytes))
    val args: Seq[Arg] = Seq(assetByteStr, addressByteStr)
    val invoke: EthereumTransaction =
      EthTxGenerator.generateEthInvoke(firstTxParticipantEthereum, secondTxParticipantAddress, "setData", args, Seq.empty, fee = 100500000L)
    val dAppInvokeIssueBalance: Long    = issueData.apply("amount").toString.toLong - scriptTransferIssueAssetNum
    val dAppAssetBalanceAfterTx: Long   = issue.quantity.value - burnNum - scriptTransferAssetNum + reissueNum
    val senderWavesBalanceAfterTx: Long = secondTxParticipantBalanceBefore - invoke.fee + scriptTransferUnitNum
    val dataEntry: Seq[DataEntry[?]] = Seq[DataEntry[?]](
      EmptyDataEntry("int"),
      BinaryDataEntry("byte", asset.id),
      BooleanDataEntry("bool", value = true),
      StringDataEntry("str", "test_string")
    )
    val actualData = Seq(
      ("int", dataMap.apply("intVal")),
      ("byte", asset.id.arr),
      ("bool", true),
      ("str", dataMap.apply("stringVal")),
      ("int", None)
    )

    "BU-. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val setScript                     = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
        val dAppBalanceBeforeInvoke: Long = firstTxParticipantBalanceBefore - issue.fee.value

        println("secondTxParticipantAddress " + secondTxParticipantAddress)
        println("firstTxParticipantAddress " + firstTxParticipantEthereumAddress)
        println("firstTxParticipantAddress " + firstTxParticipantAddress)

        withGenerateSubscription(
          SubscribeRequest.of(1, 4),
          settings = currentSettings,
          balances = Seq(
            AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(firstTxParticipantEthereumAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
          )
        ) { d =>
          d.appendBlock(setScript)
          d.appendBlock(issue)
          d.appendMicroBlock(invoke)
        } { updates =>
          val append = updates(2).append
          checkingInvoke(append, dAppBalanceBeforeInvoke)
        }
      }
    }

    def checkingInvoke(append: Append, dAppBalanceBeforeInvoke: Long): Unit = {
      val dAppWavesBalanceAfterTx: Long = dAppBalanceBeforeInvoke - scriptTransferUnitNum
      val transactionMetadata           = append.transactionsMetadata
      val invokeScript                  = transactionMetadata.head.getInvokeScript
      val arguments                     = invokeScript.arguments
      val result                        = invokeScript.result.get
      val dataEntries                   = append.transactionStateUpdates.head.dataEntries
      val invokeIssueAsset              = toVanillaAssetId(result.issues.head.assetId)
      val invokeLeaseId                 = result.leases.head.leaseId.toByteArray
      val assetDetails                  = append.transactionStateUpdates.head.assets
      val expectedValues                = List(asset.id.arr, secondTxParticipantAddress.bytes)
      val actualArguments = List(
        arguments.head.value.binaryValue.get.toByteArray,
        arguments.apply(1).value.binaryValue.get.toByteArray
      )
      checkEthereumInvokeTransaction(append.transactionIds.head, append.transactionAt(0), invoke, firstTxParticipantAddress.publicKeyHash)
      checkEthereumInvokeBaseTransactionMetadata(transactionMetadata, invoke)
      checkArguments(expectedValues, actualArguments)
      checkInvokeScriptResultData(result.data, actualData)
      checkInvokeScriptResultIssues(result.issues.head, issueData)
      checkInvokeScriptResultTransfers(result.transfers.head, secondTxParticipantAddress, scriptTransferAssetNum, asset)
      checkInvokeScriptResultTransfers(result.transfers.apply(1), secondTxParticipantAddress, scriptTransferIssueAssetNum, invokeIssueAsset)
      checkInvokeScriptResultTransfers(result.transfers.apply(2), secondTxParticipantAddress, scriptTransferUnitNum, Waves)
      checkInvokeScriptResultReissue(result.reissues.head, asset, reissueNum, reissuable = true)
      checkInvokeScriptResultBurn(result.burns.head, asset, burnNum)
      checkInvokeScriptResultSponsorFee(result.sponsorFees.head, asset, sponsorFeeAssetNum)
      checkInvokeScriptResultSponsorFee(result.sponsorFees.apply(1), invokeIssueAsset, sponsorFeeIssueAssetNum)
      checkInvokeScriptResultLease(result.leases.head, secondTxParticipantPKHash, leaseNum)
      checkInvokeScriptResultLeaseCancel(result.leaseCancels.head, invokeLeaseId)
      checkBalances(
        append.transactionStateUpdates.head.balances,
        Map(
          (secondTxParticipantAddress, Waves)            -> (secondTxParticipantBalanceBefore, senderWavesBalanceAfterTx),
          (secondTxParticipantAddress, asset)            -> (0, scriptTransferAssetNum),
          (secondTxParticipantAddress, invokeIssueAsset) -> (0, scriptTransferIssueAssetNum),
          (firstTxParticipantAddress, Waves)             -> (dAppBalanceBeforeInvoke, dAppWavesBalanceAfterTx),
          (firstTxParticipantAddress, invokeIssueAsset)  -> (0, dAppInvokeIssueBalance),
          (firstTxParticipantAddress, asset)             -> (issue.quantity.value, dAppAssetBalanceAfterTx)
        )
      )
      checkDataEntriesStateUpdate(dataEntries, dataEntry, firstTxParticipantAddress.bytes)
      checkAssetsStateUpdates(assetDetails.head.after, issueData, invokeIssueAsset, firstTxParticipant.publicKey.arr)
      checkAssetsStateUpdates(assetDetails.apply(1).before, issue, isNft = false, issue.quantity.value)
      checkAssetsStateUpdates(assetDetails.apply(1).after, issue, isNft = false, dAppAssetBalanceAfterTx + scriptTransferAssetNum)
      checkIndividualLeases(
        append.transactionStateUpdates.head.individualLeases,
        Map(
          (
            LeaseStatus.Inactive,
            leaseNum
          ) -> (invokeLeaseId, firstTxParticipant.publicKey.arr, secondTxParticipantAddress.bytes, invoke.id.value().arr)
        )
      )
    }
  }
}
