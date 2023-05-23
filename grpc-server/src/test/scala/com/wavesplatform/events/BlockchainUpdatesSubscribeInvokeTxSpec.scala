package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.*
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, CONST_STRING, EXPR}
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, StringDataEntry}
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{TxHelpers, TxNonNegativeAmount}
import com.wavesplatform.transaction.TxHelpers.{secondAddress, secondSigner}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import org.scalatest.concurrent.ScalaFutures

import java.util.concurrent.ThreadLocalRandom.current

class BlockchainUpdatesSubscribeInvokeTxSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6
  val dAppAccount: SeedKeyPair       = TxHelpers.signer(12)
  val dAppAddress: Address           = dAppAccount.toAddress
  var dAppBalanceBefore: Long        = 10.waves
  val sender: SeedKeyPair            = TxHelpers.signer(2436)
  val senderAddress: Address         = sender.toAddress
  val senderBalanceBefore: Long      = 10.waves
  var dAppBalanceAfterTx: Long       = 0L
  var dAppAssetBalanceAfterTx: Long  = 0L
  var senderBalanceAfterTx: Long     = 0L

  "BU-31. Invoke have to Return correct data for subscribe" in {
    val issue                        = TxHelpers.issue(dAppAccount)
    val asset                        = issue.asset
    val assetByteStr                 = CONST_BYTESTR(asset.id).explicitGet()
    val addressByteStr               = CONST_BYTESTR(ByteStr.apply(senderAddress.bytes)).explicitGet()
    val args: Seq[EXPR]              = Seq(assetByteStr, addressByteStr)
    val invoke                       = TxHelpers.invoke(dAppAddress, Some("setData"), args, Seq.empty, sender, fee = 100500000L)
    val dAppInvokeIssueBalance: Long = issueData.apply("amount").toString.toLong - scriptTransferIssueAssetNum
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
      val append              = updates(3).append
      val transactionMetadata = append.transactionsMetadata
      val invokeScript        = transactionMetadata.head.getInvokeScript
      val arguments           = invokeScript.arguments
      val result              = invokeScript.result.get
      val dataEntries         = append.transactionStateUpdates.head.dataEntries
      val invokeIssueAsset    = toVanillaAssetId(result.issues.head.assetId)
      val invokeLeaseId       = result.leases.head.leaseId.toByteArray
      val assetDetails        = append.transactionStateUpdates.head.assets
      val expectedValues      = List(asset.id.arr, senderAddress.bytes)
      val actualArguments = List(
        arguments.head.value.binaryValue.get.toByteArray,
        arguments.apply(1).value.binaryValue.get.toByteArray
      )

      checkInvokeTransaction(append.transactionIds.head, append.transactionAt(0), invoke, dAppAddress.publicKeyHash)
      checkInvokeBaseTransactionMetadata(transactionMetadata, invoke)
      checkArguments(expectedValues, actualArguments)
      checkInvokeScriptResultData(result.data, actualData)
      checkInvokeScriptResultIssues(result.issues.head, issueData)
      checkInvokeScriptResultTransfers(result.transfers.head, senderAddress, scriptTransferAssetNum, asset)
      checkInvokeScriptResultTransfers(result.transfers.apply(1), senderAddress, scriptTransferIssueAssetNum, invokeIssueAsset)
      checkInvokeScriptResultTransfers(result.transfers.apply(2), senderAddress, scriptTransferUnitNum, Waves)
      checkInvokeScriptResultReissue(result.reissues.head, asset, reissueNum, reissuable = true)
      checkInvokeScriptResultBurn(result.burns.head, asset, burnNum)
      checkInvokeScriptResultSponsorFee(result.sponsorFees.head, asset, sponsorFeeAssetNum)
      checkInvokeScriptResultSponsorFee(result.sponsorFees.apply(1), invokeIssueAsset, sponsorFeeIssueAssetNum)
      checkInvokeScriptResultLease(result.leases.head, senderAddress.publicKeyHash, leaseNum)
      checkInvokeScriptResultLeaseCancel(result.leaseCancels.head, invokeLeaseId)


      checkBalances(
        append.transactionStateUpdates.head.balances,
        Map(
          (senderAddress, Waves)            -> (senderBalanceBefore, senderBalanceAfterTx),
          (senderAddress, asset)            -> (0, scriptTransferAssetNum),
          (senderAddress, invokeIssueAsset) -> (0, scriptTransferIssueAssetNum),
          (dAppAddress, Waves)              -> (dAppBalanceBefore, dAppBalanceAfterTx),
          (dAppAddress, invokeIssueAsset)   -> (0, dAppInvokeIssueBalance),
          (dAppAddress, asset)              -> (issue.quantity.value, dAppAssetBalanceAfterTx)
        )
      )
      checkDataEntriesStateUpdate(dataEntries, dataEntry, dAppAddress.bytes)
      checkAssetsStateUpdates(assetDetails.head.after, issueData, invokeIssueAsset, dAppAccount.publicKey.arr)
      checkAssetsStateUpdates(assetDetails.apply(1).before, issue, isNft = false, issue.quantity.value)
      checkAssetsStateUpdates(assetDetails.apply(1).after, issue, isNft = false, dAppAssetBalanceAfterTx + scriptTransferAssetNum)
      checkIndividualLeases(
        append.transactionStateUpdates.head.individualLeases,
        Map(
          (
            LeaseStatus.Inactive,
            leaseNum
          ) -> (invokeLeaseId, dAppAccount.publicKey.arr, senderAddress.bytes, invoke.id.value().arr)
        )
      )
    }
  }

  "BU- case: doubles nested i.caller. Invoke have to return correct data for subscribe" in {
    val assetDappAccount: SeedKeyPair = TxHelpers.signer(current.nextInt(3000, 22001))
    val assetDappAddress: Address     = assetDappAccount.toAddress
    val assetDappBalance: Long        = 10.waves
    val secondAddressBalance: Long    = 10.waves

    val invokerDappAccount: SeedKeyPair = TxHelpers.signer(current.nextInt(22002, 39235))
    val invokerDappAddress: Address     = invokerDappAccount.toAddress
    val invokerDappBalance: Long        = 4.waves

    val issue = TxHelpers.issue(assetDappAccount)
    val asset = issue.asset
    val massTx = TxHelpers.massTransfer(
      assetDappAccount,
      Seq(
        ParsedTransfer(dAppAddress, TxNonNegativeAmount.unsafeFrom(100000000)),
        ParsedTransfer(secondAddress, TxNonNegativeAmount.unsafeFrom(200000000)),
        ParsedTransfer(invokerDappAddress, TxNonNegativeAmount.unsafeFrom(300000000))
      ),
      issue.asset,
      fee = 500000
    )

    val secondAddressByteStr    = CONST_BYTESTR(ByteStr.apply(secondAddress.bytes)).explicitGet()
    val assetDappAddressByteStr = CONST_BYTESTR(ByteStr.apply(assetDappAddress.bytes)).explicitGet()
    val assetByteStr            = CONST_BYTESTR(asset.id).explicitGet()
    val invokeStr               = CONST_STRING(bar).explicitGet()
    val invokeInt               = CONST_LONG(scriptTransferUnitNum)

    val args: Seq[EXPR] = Seq(secondAddressByteStr, assetDappAddressByteStr, invokeInt, invokeStr, assetByteStr)

    val mainDAppTx         = TxHelpers.setScript(dAppAccount, TxHelpers.script(mainDAppScript))
    val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.caller")))
    val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.caller")))

    val invoke = TxHelpers.invoke(dAppAddress, Some("foo"), args, Seq.empty, invokerDappAccount, fee = 100500000L)
    val actualData = Seq(
      ("bar", scriptTransferUnitNum * 2)
    )

    withGenerateSubscription(
      settings = currentSettings,
      balances = Seq(
        AddrWithBalance(dAppAddress, dAppBalanceBefore),
        AddrWithBalance(secondAddress, secondAddressBalance),
        AddrWithBalance(invokerDappAddress, invokerDappBalance),
        AddrWithBalance(assetDappAddress, assetDappBalance)
      )
    ) { d =>
      d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
      d.appendMicroBlock(invoke)
    } { updates =>
      val append              = updates(2).append
      val transactionMetadata = append.transactionsMetadata
      val invokeScript        = transactionMetadata.head.getInvokeScript
      val arguments           = invokeScript.arguments
      val result              = invokeScript.result
      val expectedValues: List[Any] = List(secondAddress.bytes, assetDappAddress.bytes, scriptTransferUnitNum, bar, asset.id.arr)
      val actualArguments: List[Any] = List(
        arguments.head.value.binaryValue.get.toByteArray,
        arguments(1).value.binaryValue.get.toByteArray,
        arguments(2).value.integerValue.get,
        arguments(3).value.stringValue.get,
        arguments(4).value.binaryValue.get.toByteArray,
      )

      checkInvokeTransaction(append.transactionIds.head, append.transactionAt(0), invoke, dAppAddress.publicKeyHash)
      checkInvokeBaseTransactionMetadata(transactionMetadata, invoke)
      checkArguments(expectedValues, actualArguments)
      checkInvokeScriptResultData(result.get.data, actualData)

    }
  }
}


