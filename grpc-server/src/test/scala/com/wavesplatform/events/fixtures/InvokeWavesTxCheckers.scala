package com.wavesplatform.events.fixtures

import com.wavesplatform.account.Address
import com.wavesplatform.events.BlockchainUpdatesTestBase
import com.wavesplatform.events.BlockchainUpdatesTestBase.filterOutMinerBalanceUpdates
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.*
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.events.protobuf.TransactionMetadata.InvokeScriptMetadata
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.secondAddress
import com.wavesplatform.transaction.assets.IssueTransaction
import org.scalactic.source.Position

object InvokeWavesTxCheckers extends BlockchainUpdatesTestBase {
  def checkSimpleInvoke(
      append: Append,
      issue: IssueTransaction,
      issuerAssetBalanceAfterTx: Long,
      invokeScript: InvokeScriptMetadata,
      invokeId: Array[Byte]
  ): Unit = {
    val assetId = issue.assetId.arr

    val dataEntry: Seq[DataEntry[?]] = Seq[DataEntry[?]](
      EmptyDataEntry("int"),
      BinaryDataEntry("byte", issue.asset.id),
      BooleanDataEntry("bool", value = true),
      StringDataEntry("str", "test_string")
    )
    val actualData = Seq(
      ("int", dataMap.apply("intVal")),
      ("byte", assetId),
      ("bool", true),
      ("str", dataMap.apply("stringVal")),
      ("int", None)
    )

    val arguments               = invokeScript.arguments
    val result                  = invokeScript.result.get
    val invokeIssueAsset        = toVanillaAssetId(result.issues.head.assetId)
    val invokeLeaseId           = result.leases.head.leaseId.toByteArray
    val dataEntries             = append.transactionStateUpdates.head.dataEntries
    val assetDetails            = append.transactionStateUpdates.head.assets
    val expectedArgumentsValues = List(assetId, secondTxParticipantAddress.bytes)
    val actualArgumentsValues = List(
      arguments.head.value.binaryValue.get.toByteArray,
      arguments.apply(1).value.binaryValue.get.toByteArray
    )

    checkArguments(expectedArgumentsValues, actualArgumentsValues)
    checkInvokeScriptResultData(result.data, actualData)
    checkInvokeScriptResultIssues(result.issues.head, issueData)
    checkInvokeScriptResultTransfers(result.transfers.head, secondTxParticipantAddress, scriptTransferAssetNum, issue.asset)
    checkInvokeScriptResultTransfers(result.transfers.apply(1), secondTxParticipantAddress, scriptTransferIssueAssetNum, invokeIssueAsset)
    checkInvokeScriptResultTransfers(result.transfers.apply(2), secondTxParticipantAddress, scriptTransferUnitNum, Waves)
    checkInvokeScriptResultReissue(result.reissues.head, issue.asset, reissueNum, reissuable = true)
    checkInvokeScriptResultBurn(result.burns.head, issue.asset, burnNum)
    checkInvokeScriptResultSponsorFee(result.sponsorFees.head, issue.asset, sponsorFeeAssetNum)
    checkInvokeScriptResultSponsorFee(result.sponsorFees.apply(1), invokeIssueAsset, sponsorFeeIssueAssetNum)
    checkInvokeScriptResultLease(result.leases.head, secondTxParticipantPKHash, leaseNum)
    checkInvokeScriptResultLeaseCancel(result.leaseCancels.head, invokeLeaseId)
    checkDataEntriesStateUpdate(dataEntries, dataEntry, firstTxParticipantAddress.bytes)
    checkAssetsStateUpdates(assetDetails.head.after, issueData, invokeIssueAsset, firstTxParticipant.publicKey.arr)
    checkAssetsStateUpdates(assetDetails.apply(1).before, issue, isNft = false, issue.quantity.value)
    checkAssetsStateUpdates(assetDetails.apply(1).after, issue, isNft = false, issuerAssetBalanceAfterTx + scriptTransferAssetNum)
    checkIndividualLeases(
      append.transactionStateUpdates.head.individualLeases,
      Map(
        (
          LeaseStatus.Inactive,
          leaseNum
        ) -> (invokeLeaseId, firstTxParticipant.publicKey.arr, secondTxParticipantAddress.bytes, invokeId)
      )
    )
  }

  def checkDoubleNestingInvoke(
      append: Append,
      invokeScript: InvokeScriptMetadata,
      issue: IssueTransaction,
      assetDappAddress: Address,
      nestedTransferAddress: Address,
      doubleNestedTransferAddress: Address
  ): Unit = {
    val scriptTransferWavesSum               = scriptTransferUnitNum * 2
    val asset                                = issue.asset
    val actualData                           = Seq(("bar", scriptTransferWavesSum))
    val arguments                            = invokeScript.arguments
    val result                               = invokeScript.result.get
    val invokes                              = result.invokes.head
    val invokesStateChangeInvoke             = invokes.stateChanges.get.invokes.head
    val actualDataEntries                    = append.transactionStateUpdates.head.dataEntries
    val expectDataEntries: Seq[DataEntry[?]] = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferWavesSum))
    val expectedValues: List[Any]            = List(secondAddress.bytes, assetDappAddress.bytes, scriptTransferUnitNum, bar, issue.asset.id.arr)
    val actualArguments: List[Any] = List(
      arguments.head.value.binaryValue.get.toByteArray,
      arguments(1).value.binaryValue.get.toByteArray,
      arguments(2).value.integerValue.get,
      arguments(3).value.stringValue.get,
      arguments(4).value.binaryValue.get.toByteArray
    )

    checkArguments(expectedValues, actualArguments)
    checkInvokeScriptResultData(result.data, actualData)
    checkInvokeScriptBaseInvokes(invokes, secondAddress, bar)
    checkInvokeScriptInvokesArgs(invokes.call.get.args.head, scriptTransferUnitNum)
    checkInvokeScriptInvokesArgs(invokes.call.get.args.apply(1), asset.id.arr)
    checkInvokeScriptInvokesArgs(invokes.call.get.args.apply(2), assetDappAddress.bytes)
    checkInvokeScriptInvokesPayments(invokes.payments.head, asset, paymentNum)
    checkInvokeScriptResultTransfers(invokes.stateChanges.get.transfers.head, nestedTransferAddress, scriptTransferAssetNum, asset)
    checkInvokeScriptBaseInvokes(invokesStateChangeInvoke, assetDappAddress, baz)
    checkInvokeScriptInvokesArgs(invokesStateChangeInvoke.call.get.args.head, scriptTransferUnitNum)
    checkInvokeScriptResultTransfers(
      invokesStateChangeInvoke.stateChanges.get.transfers.head,
      doubleNestedTransferAddress,
      scriptTransferUnitNum,
      Waves
    )
    checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, firstTxParticipantAddress.bytes)
  }

  def checkInvokeDoubleNestedBlockchainUpdates(
      append: Append,
      invokeScript: InvokeScriptMetadata,
      transferAddress: Address,
      nestedTransferAddress: Address,
      doubleNestedTransferAddress: Address,
      issue: IssueTransaction,
      expectBalancesMap: Map[(Address, Asset), (Long, Long)]
  )(implicit pos: Position): Unit = {
    val actualDataEntries = append.transactionStateUpdates.head.dataEntries
    checkDoubleNestingInvoke(append, invokeScript, issue, transferAddress, nestedTransferAddress, doubleNestedTransferAddress)
    checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, firstTxParticipantAddress.bytes)

    checkBalances(filterOutMinerBalanceUpdates(append), expectBalancesMap)
  }
}
