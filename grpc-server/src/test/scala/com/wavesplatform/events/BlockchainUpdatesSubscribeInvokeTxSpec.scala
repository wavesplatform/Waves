package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.*
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, CONST_STRING, EXPR}
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.{defaultAddress, script, secondAddress, secondSigner, setScript}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{TxHelpers, TxNonNegativeAmount}
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeInvokeTxSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6
  val dAppAccount: SeedKeyPair       = TxHelpers.signer(2)
  val sender: SeedKeyPair            = TxHelpers.signer(3)
  val senderAddress: Address         = sender.toAddress
  val dAppAddress: Address           = dAppAccount.toAddress
  val dAppBalanceBefore: Long        = 10.waves
  val senderWavesBalanceBefore: Long = 8.waves

  "BU-31. Invoke have to return correct data for subscribe" in {
    for (libVersion <- 5 to 6) {
      val issue                           = TxHelpers.issue(dAppAccount)
      val setScript                       = TxHelpers.setScript(dAppAccount, TxHelpers.script(invokeAssetScript(libVersion)))
      val asset                           = issue.asset
      val assetByteStr                    = CONST_BYTESTR(asset.id).explicitGet()
      val addressByteStr                  = CONST_BYTESTR(ByteStr.apply(senderAddress.bytes)).explicitGet()
      val args: Seq[EXPR]                 = Seq(assetByteStr, addressByteStr)
      val invoke                          = TxHelpers.invoke(dAppAddress, Some("setData"), args, Seq.empty, sender, fee = 100500000L)
      val dAppInvokeIssueBalance: Long    = issueData.apply("amount").toString.toLong - scriptTransferIssueAssetNum
      val dAppBalanceBeforeInvoke: Long   = dAppBalanceBefore - issue.fee.value - setScript.fee.value
      val dAppWavesBalanceAfterTx: Long   = dAppBalanceBeforeInvoke - scriptTransferUnitNum
      val dAppAssetBalanceAfterTx: Long   = issue.quantity.value - burnNum - scriptTransferAssetNum + reissueNum
      val senderWavesBalanceAfterTx: Long = senderWavesBalanceBefore - invoke.fee.value + scriptTransferUnitNum
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
          AddrWithBalance(senderAddress, senderWavesBalanceBefore)
        )
      ) { d =>
        d.appendBlock(setScript)
        d.appendBlock(issue)
        d.appendMicroBlock(invoke)
      } { updates =>
        val append              = updates(3).append
        val transactionMetadata = append.transactionsMetadata.head
        val invokeScript        = transactionMetadata.getInvokeScript
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
        val minerBalanceBeforeInvoke = 3 * 6.waves + setScript.fee.value + issue.fee.value * 2 / 5
        val minerBalanceAfterInvoke  = minerBalanceBeforeInvoke + invoke.fee.value * 2 / 5
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
            (senderAddress, Waves)            -> (senderWavesBalanceBefore, senderWavesBalanceAfterTx),
            (senderAddress, asset)            -> (0, scriptTransferAssetNum),
            (senderAddress, invokeIssueAsset) -> (0, scriptTransferIssueAssetNum),
            (dAppAddress, Waves)              -> (dAppBalanceBeforeInvoke, dAppWavesBalanceAfterTx),
            (dAppAddress, invokeIssueAsset)   -> (0, dAppInvokeIssueBalance),
            (dAppAddress, asset)              -> (issue.quantity.value, dAppAssetBalanceAfterTx),
            (defaultAddress, Waves)           -> (minerBalanceBeforeInvoke, minerBalanceAfterInvoke)
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
  }

  "Double nesting call tests" - {
    val assetDappAccount: SeedKeyPair   = TxHelpers.signer(4)
    val assetDappAddress: Address       = assetDappAccount.toAddress
    val invokerDappAccount: SeedKeyPair = TxHelpers.signer(5)
    val invokerDappAddress: Address     = invokerDappAccount.toAddress
    val issue                           = TxHelpers.issue(assetDappAccount)
    val asset                           = issue.asset
    val assetTransferAmount: Long       = 200000000L
    val massTx = TxHelpers.massTransfer(
      assetDappAccount,
      Seq(
        ParsedTransfer(dAppAddress, TxNonNegativeAmount.unsafeFrom(assetTransferAmount)),
        ParsedTransfer(secondAddress, TxNonNegativeAmount.unsafeFrom(assetTransferAmount)),
        ParsedTransfer(invokerDappAddress, TxNonNegativeAmount.unsafeFrom(assetTransferAmount))
      ),
      asset,
      fee = 500000
    )
    val args: Seq[EXPR] =
      Seq(
        CONST_BYTESTR(ByteStr.apply(secondAddress.bytes)).explicitGet(),
        CONST_BYTESTR(ByteStr.apply(assetDappAddress.bytes)).explicitGet(),
        CONST_LONG(scriptTransferUnitNum),
        CONST_STRING(bar).explicitGet(),
        CONST_BYTESTR(asset.id).explicitGet()
      )
    val actualData                 = Seq(("bar", scriptTransferUnitNum * 2))
    val invoke                     = TxHelpers.invoke(dAppAddress, Some("foo"), args, Seq.empty, invokerDappAccount, fee = 100500000L)
    val invokerDappBalance: Long   = 4.waves
    val secondAddressBalance: Long = 8.waves
    val assetDappBalance: Long     = 12.waves

    "BU-77. doubles nested i.caller. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val scriptTransferWavesSum             = scriptTransferUnitNum * 2
        val mainDAppTx                         = setScript(dAppAccount, script(mainDAppScript(libVersion)))
        val nestedDAppTx                       = setScript(secondSigner, script(nestedDAppScript("i.caller", libVersion)))
        val doubleNestedDAppTx                 = setScript(assetDappAccount, script(doubleNestedDAppScript("i.caller", libVersion)))
        val expectDataEntries                  = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferWavesSum))
        val secondAddressWavesBalanceBefore    = secondAddressBalance - nestedDAppTx.fee.value
        val secondAddressWavesBalanceAfter     = secondAddressWavesBalanceBefore + scriptTransferUnitNum
        val assetDappAddressWavesBalanceBefore = assetDappBalance - issue.fee.value - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter  = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val invokerDappAddressWavesBalance     = invokerDappBalance - invoke.fee.value
        val secondAddressAssetBalance          = assetTransferAmount - scriptTransferAssetNum + paymentNum
        val dAppAddressAssetBalance            = assetTransferAmount + scriptTransferAssetNum - paymentNum
        val txsBeforeInvoke                    = Seq(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
        val minerBalanceBeforeInvoke           = 2 * 6.waves + txsBeforeInvoke.map(_.fee.value).sum * 2 / 5
        val minerBalanceAfterInvoke            = minerBalanceBeforeInvoke + invoke.fee.value * 2 / 5

        withAddedBlocksAndSubscribe(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val actualDataEntries = updates(2).getAppend.transactionStateUpdates.head.dataEntries
          checkInvokeDoubleNestedBlockchainUpdates(updates(2).getAppend, dAppAddress, secondAddress)
          checkBalances(
            updates(2).getAppend.transactionStateUpdates.head.balances,
            Map(
              (secondAddress, Waves)      -> (secondAddressWavesBalanceBefore, secondAddressWavesBalanceAfter),
              (secondAddress, asset)      -> (assetTransferAmount, secondAddressAssetBalance),
              (dAppAddress, asset)        -> (assetTransferAmount, dAppAddressAssetBalance),
              (assetDappAddress, Waves)   -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (invokerDappAddress, Waves) -> (invokerDappBalance, invokerDappAddressWavesBalance),
              (defaultAddress, Waves)     -> (minerBalanceBeforeInvoke, minerBalanceAfterInvoke)
            )
          )
          checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, dAppAddress.bytes)
        }
      }
    }

    "BU-39. double nested i.originCaller. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx                         = setScript(dAppAccount, script(mainDAppScript(libVersion)))
        val nestedDAppTx                       = setScript(secondSigner, script(nestedDAppScript("i.originCaller", libVersion)))
        val doubleNestedDAppTx                 = setScript(assetDappAccount, script(doubleNestedDAppScript("i.originCaller", libVersion)))
        val expectDataEntries                  = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferUnitNum * 2))
        val assetDappAddressWavesBalanceBefore = assetDappBalance - issue.fee.value - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter  = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val invokerDappAddressWavesBalance     = invokerDappBalance - invoke.fee.value + scriptTransferUnitNum
        val invokerDappAddressAssetBalance     = assetTransferAmount + scriptTransferAssetNum
        val secondAddressAssetBalance          = assetTransferAmount - scriptTransferAssetNum + paymentNum
        val dAppAddressAssetBalance            = assetTransferAmount - paymentNum
        val txsBeforeInvoke                    = Seq(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
        val minerBalanceBeforeInvoke           = 2 * 6.waves + txsBeforeInvoke.map(_.fee.value).sum * 2 / 5
        val minerBalanceAfterInvoke            = minerBalanceBeforeInvoke + invoke.fee.value * 2 / 5

        withAddedBlocksAndSubscribe(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val actualDataEntries = updates(2).getAppend.transactionStateUpdates.head.dataEntries
          checkInvokeDoubleNestedBlockchainUpdates(updates(2).getAppend, invokerDappAddress, invokerDappAddress)
          checkBalances(
            updates(2).getAppend.transactionStateUpdates.head.balances,
            Map(
              (invokerDappAddress, asset) -> (assetTransferAmount, invokerDappAddressAssetBalance),
              (secondAddress, asset)      -> (assetTransferAmount, secondAddressAssetBalance),
              (dAppAddress, asset)        -> (assetTransferAmount, dAppAddressAssetBalance),
              (assetDappAddress, Waves)   -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (invokerDappAddress, Waves) -> (invokerDappBalance, invokerDappAddressWavesBalance),
              (defaultAddress, Waves)     -> (minerBalanceBeforeInvoke, minerBalanceAfterInvoke)
            )
          )
          checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, dAppAddress.bytes)
        }
      }
    }

    def withAddedBlocksAndSubscribe(
        mainDAppTx: SetScriptTransaction,
        nestedDAppTx: SetScriptTransaction,
        doubleNestedDAppTx: SetScriptTransaction
    )(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
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
        f(updates)
      }
    }

    def checkInvokeDoubleNestedBlockchainUpdates(
        append: Append,
        nestedTransferAddress: Address,
        doubleNestedTransferAddress: Address
    ): Unit = {
      val arguments                 = append.transactionsMetadata.head.getInvokeScript.arguments
      val result                    = append.transactionsMetadata.head.getInvokeScript.result.get
      val invokes                   = result.invokes.head
      val invokesStateChangeInvoke  = invokes.stateChanges.get.invokes.head
      val expectedValues: List[Any] = List(secondAddress.bytes, assetDappAddress.bytes, scriptTransferUnitNum, bar, asset.id.arr)
      val actualArguments: List[Any] = List(
        arguments.head.value.binaryValue.get.toByteArray,
        arguments(1).value.binaryValue.get.toByteArray,
        arguments(2).value.integerValue.get,
        arguments(3).value.stringValue.get,
        arguments(4).value.binaryValue.get.toByteArray
      )
      checkInvokeTransaction(append.transactionIds.head, append.transactionAt(0), invoke, dAppAddress.publicKeyHash)
      checkInvokeBaseTransactionMetadata(append.transactionsMetadata.head, invoke)
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
    }
  }
}
