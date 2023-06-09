package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdatesRangeRequest, SubscribeRequest}
import com.wavesplatform.events.fixtures.InvokeWavesTxCheckers.{checkDoubleNestingInvoke, checkSimpleInvoke}
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.*
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, CONST_STRING, EXPR}
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.state.{DataEntry, IntegerDataEntry}
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{TxHelpers, TxNonNegativeAmount}
import com.wavesplatform.transaction.TxHelpers.{secondAddress, secondSigner}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer

class BlockchainUpdatesInvokeTxSpec extends BlockchainUpdatesTestBase with WithBUDomain {
  "Simple invoke transaction" - {
    val issue           = TxHelpers.issue(firstTxParticipant)
    val asset           = issue.asset
    val assetByteStr    = CONST_BYTESTR(asset.id).explicitGet()
    val addressByteStr  = CONST_BYTESTR(ByteStr.apply(secondTxParticipantAddress.bytes)).explicitGet()
    val args: Seq[EXPR] = Seq(assetByteStr, addressByteStr)
    val invoke = TxHelpers.invoke(firstTxParticipantAddress, Some(invokeFunctionName), args, Seq.empty, secondTxParticipant, fee = 100500000L)
    val dAppInvokeIssueBalance: Long    = issueData.apply("amount").toString.toLong - scriptTransferIssueAssetNum
    val dAppAssetBalanceAfterTx: Long   = issue.quantity.value - burnNum - scriptTransferAssetNum + reissueNum
    val senderWavesBalanceAfterTx: Long = secondTxParticipantBalanceBefore - invoke.fee.value + scriptTransferUnitNum

    "BU-31. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val setScript                     = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
        val dAppBalanceBeforeInvoke: Long = firstTxParticipantBalanceBefore - issue.fee.value - setScript.fee.value

        withGenerateSubscription(
          SubscribeRequest.of(1, 4),
          settings = currentSettings,
          balances = Seq(
            AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
          )
        ) { d =>
          d.appendBlock(setScript)
          d.appendBlock(issue)
          d.appendMicroBlock(invoke)
        } { updates =>
          val append = updates(3).append
          checkWavesInvoke(append, dAppBalanceBeforeInvoke)
        }
      }
    }

    "BU-208. Invoke have to return correct data for getBlockUpdate" in {
      for (libVersion <- 5 to 6) {
        val setScript                     = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
        val dAppBalanceBeforeInvoke: Long = firstTxParticipantBalanceBefore - issue.fee.value - setScript.fee.value
        withGenerateGetBlockUpdate(
          height = 4,
          settings = currentSettings,
          balances = Seq(
            AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
          )
        ) { d =>
          d.appendBlock(setScript)
          d.appendBlock(issue)
          d.appendBlock(invoke)
        } { getBlockUpdate =>
          val append = getBlockUpdate.getUpdate.getAppend
          checkWavesInvoke(append, dAppBalanceBeforeInvoke)
        }
      }
    }

    "BU-173. Invoke have to return correct data for getBlockUpdateRange" in {
      for (libVersion <- 5 to 6) {
        val setScript                     = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
        val dAppBalanceBeforeInvoke: Long = firstTxParticipantBalanceBefore - issue.fee.value - setScript.fee.value
        withGenerateGetBlockUpdateRange(
          GetBlockUpdatesRangeRequest.of(1, 4),
          settings = currentSettings,
          balances = Seq(
            AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
          )
        ) { d =>
          d.appendBlock(setScript)
          d.appendBlock(issue)
          d.appendBlock(invoke)
          d.appendBlock()
        } { getBlockUpdateRange =>
          val append = getBlockUpdateRange.apply(3).getAppend
          checkWavesInvoke(append, dAppBalanceBeforeInvoke)
        }
      }
    }

    def checkWavesInvoke(append: Append, dAppBalanceBeforeInvoke: Long): Unit = {
      val dAppWavesBalanceAfterTx: Long = dAppBalanceBeforeInvoke - scriptTransferUnitNum
      val transactionMetadata           = append.transactionsMetadata.head
      val invokeScript                  = transactionMetadata.getInvokeScript
      val result                        = invokeScript.result.get
      val invokeIssueAsset              = toVanillaAssetId(result.issues.head.assetId)
      val invokeLeaseId                 = result.leases.head.leaseId.toByteArray

      checkInvokeTransaction(append.transactionIds.head, append.transactionAt(0), invoke, firstTxParticipantAddress.publicKeyHash)
      checkInvokeBaseTransactionMetadata(transactionMetadata, invoke)
      checkSimpleInvoke(append, issue, dAppAssetBalanceAfterTx, invokeScript)
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

  "Double nesting call tests" - {
    val assetDappAccount: SeedKeyPair   = TxHelpers.signer(4)
    val assetDappAddress: Address       = assetDappAccount.toAddress
    val invokerDappAccount: SeedKeyPair = TxHelpers.signer(5)
    val invokerDappAddress: Address     = invokerDappAccount.toAddress
    val issue                           = TxHelpers.issue(assetDappAccount)
    val asset                           = issue.asset
    val issueAssetFee                   = issue.fee.value
    val massTx = TxHelpers.massTransfer(
      assetDappAccount,
      Seq(
        ParsedTransfer(firstTxParticipantAddress, TxNonNegativeAmount.unsafeFrom(amount)),
        ParsedTransfer(secondAddress, TxNonNegativeAmount.unsafeFrom(amount)),
        ParsedTransfer(invokerDappAddress, TxNonNegativeAmount.unsafeFrom(amount))
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
    val invoke                     = TxHelpers.invoke(firstTxParticipantAddress, Some("foo"), args, Seq.empty, invokerDappAccount, fee = 100500000L)
    val invokerDappBalance: Long   = 4.waves
    val secondAddressBalance: Long = 8.waves
    val assetDappBalance: Long     = 12.waves
    val scriptTransferWavesSum     = scriptTransferUnitNum * 2
    val secondAddressAssetBalanceForAll               = amount - scriptTransferAssetNum + paymentNum
    val dAppAddressAssetBalanceForCaller              = amount + scriptTransferAssetNum - paymentNum
    val invokerDappAddressAssetBalanceForOriginCaller = amount + scriptTransferAssetNum
    val dAppAddressAssetBalanceForOriginCaller        = amount - paymentNum

    "BU-77. doubles nested i.caller. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.caller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.caller", libVersion)))
        val expectDataEntries: Seq[DataEntry[?]] = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferWavesSum))
        val secondAddressWavesBalanceBefore      = secondAddressBalance - nestedDAppTx.fee.value
        val secondAddressWavesBalanceAfter       = secondAddressWavesBalanceBefore + scriptTransferUnitNum
        val assetDappAddressWavesBalanceBefore   = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter    = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val invokerDappAddressWavesBalance       = invokerDappBalance - invoke.fee.value

        withAddedBlocksAndSubscribeDoubleNestingInvoke(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val actualDataEntries = updates(2).getAppend.transactionStateUpdates.head.dataEntries
          checkInvokeDoubleNestedBlockchainUpdates(updates(2).getAppend, assetDappAddress, firstTxParticipantAddress, secondAddress)
          checkBalances(
            updates(2).getAppend.transactionStateUpdates.head.balances,
            Map(
              (secondAddress, Waves)             -> (secondAddressWavesBalanceBefore, secondAddressWavesBalanceAfter),
              (secondAddress, asset)             -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset) -> (amount, dAppAddressAssetBalanceForCaller),
              (assetDappAddress, Waves)          -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (invokerDappAddress, Waves)        -> (invokerDappBalance, invokerDappAddressWavesBalance)
            )
          )
          checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, firstTxParticipantAddress.bytes)
        }
      }
    }

    "BU-210. doubles nested i.caller. Invoke have to return correct data for getBlockUpdate" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.caller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.caller", libVersion)))
        val expectDataEntries: Seq[DataEntry[?]] = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferWavesSum))
        val secondAddressWavesBalanceBefore      = secondAddressBalance - nestedDAppTx.fee.value
        val secondAddressWavesBalanceAfter       = secondAddressWavesBalanceBefore + scriptTransferUnitNum
        val assetDappAddressWavesBalanceBefore   = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter    = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val invokerDappAddressWavesBalance       = invokerDappBalance - invoke.fee.value

        withAddedBlocksAndGetBlockUpdate(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { update =>
          val actualDataEntries = update.getAppend.transactionStateUpdates.head.dataEntries
          checkInvokeDoubleNestedBlockchainUpdates(update.getAppend, assetDappAddress, firstTxParticipantAddress, secondAddress)
          checkBalances(
            update.getAppend.transactionStateUpdates.head.balances,
            Map(
              (secondAddress, Waves)             -> (secondAddressWavesBalanceBefore, secondAddressWavesBalanceAfter),
              (secondAddress, asset)             -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset) -> (amount, dAppAddressAssetBalanceForCaller),
              (assetDappAddress, Waves)          -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (invokerDappAddress, Waves)        -> (invokerDappBalance, invokerDappAddressWavesBalance)
            )
          )
          checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, firstTxParticipantAddress.bytes)
        }
      }
    }

    "BU-175. doubles nested i.caller. Invoke have to return correct data for getBlockUpdateRange" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.caller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.caller", libVersion)))
        val expectDataEntries: Seq[DataEntry[?]] = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferWavesSum))
        val secondAddressWavesBalanceBefore      = secondAddressBalance - nestedDAppTx.fee.value
        val secondAddressWavesBalanceAfter       = secondAddressWavesBalanceBefore + scriptTransferUnitNum
        val assetDappAddressWavesBalanceBefore   = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter    = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val invokerDappAddressWavesBalance       = invokerDappBalance - invoke.fee.value

        withAddedBlocksAndGetBlockUpdateRange(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val actualDataEntries = updates(2).getAppend.transactionStateUpdates.head.dataEntries
          checkInvokeDoubleNestedBlockchainUpdates(updates(2).getAppend, assetDappAddress, firstTxParticipantAddress, secondAddress)
          checkBalances(
            updates(2).getAppend.transactionStateUpdates.head.balances,
            Map(
              (secondAddress, Waves)             -> (secondAddressWavesBalanceBefore, secondAddressWavesBalanceAfter),
              (secondAddress, asset)             -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset) -> (amount, dAppAddressAssetBalanceForCaller),
              (assetDappAddress, Waves)          -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (invokerDappAddress, Waves)        -> (invokerDappBalance, invokerDappAddressWavesBalance)
            )
          )
          checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, firstTxParticipantAddress.bytes)
        }
      }
    }

    "BU-39. double nested i.originCaller. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.originCaller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.originCaller", libVersion)))
        val expectDataEntries: Seq[DataEntry[?]] = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferUnitNum * 2))
        val assetDappAddressWavesBalanceBefore   = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter    = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val invokerDappAddressWavesBalance       = invokerDappBalance - invoke.fee.value + scriptTransferUnitNum
        withAddedBlocksAndSubscribeDoubleNestingInvoke(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val actualDataEntries = updates(2).getAppend.transactionStateUpdates.head.dataEntries
          checkInvokeDoubleNestedBlockchainUpdates(updates(2).getAppend, assetDappAddress, invokerDappAddress, invokerDappAddress)
          checkBalances(
            updates(2).getAppend.transactionStateUpdates.head.balances,
            Map(
              (invokerDappAddress, asset)        -> (amount, invokerDappAddressAssetBalanceForOriginCaller),
              (secondAddress, asset)             -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset) -> (amount, dAppAddressAssetBalanceForOriginCaller),
              (assetDappAddress, Waves)          -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (invokerDappAddress, Waves)        -> (invokerDappBalance, invokerDappAddressWavesBalance)
            )
          )
          checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, firstTxParticipantAddress.bytes)
        }
      }
    }

    "BU-209. doubles nested i.originCaller. Invoke have to return correct data for getBlockUpdate" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.originCaller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.originCaller", libVersion)))
        val expectDataEntries: Seq[DataEntry[?]] = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferWavesSum))
        val assetDappAddressWavesBalanceBefore   = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter    = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val invokerDappAddressWavesBalance       = invokerDappBalance - invoke.fee.value + scriptTransferUnitNum

        withAddedBlocksAndGetBlockUpdate(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { update =>
          val actualDataEntries = update.getAppend.transactionStateUpdates.head.dataEntries
          checkInvokeDoubleNestedBlockchainUpdates(update.getAppend, assetDappAddress, invokerDappAddress, invokerDappAddress)
          checkBalances(
            update.getAppend.transactionStateUpdates.head.balances,
            Map(
              (invokerDappAddress, asset)        -> (amount, invokerDappAddressAssetBalanceForOriginCaller),
              (secondAddress, asset)             -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset) -> (amount, dAppAddressAssetBalanceForOriginCaller),
              (assetDappAddress, Waves)          -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (invokerDappAddress, Waves)        -> (invokerDappBalance, invokerDappAddressWavesBalance)
            )
          )
          checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, firstTxParticipantAddress.bytes)
        }
      }
    }

    "BU-174. doubles nested i.originCaller. Invoke have to return correct data for getBlockUpdateRange" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.originCaller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.originCaller", libVersion)))
        val expectDataEntries: Seq[DataEntry[?]] = Seq[DataEntry[?]](IntegerDataEntry(bar, scriptTransferWavesSum))
        val assetDappAddressWavesBalanceBefore   = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter    = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val invokerDappAddressWavesBalance       = invokerDappBalance - invoke.fee.value + scriptTransferUnitNum

        withAddedBlocksAndGetBlockUpdateRange(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val actualDataEntries = updates(2).getAppend.transactionStateUpdates.head.dataEntries
          checkInvokeDoubleNestedBlockchainUpdates(updates(2).getAppend, assetDappAddress, invokerDappAddress, invokerDappAddress)
          checkBalances(
            updates(2).getAppend.transactionStateUpdates.head.balances,
            Map(
              (invokerDappAddress, asset)        -> (amount, invokerDappAddressAssetBalanceForOriginCaller),
              (secondAddress, asset)             -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset) -> (amount, dAppAddressAssetBalanceForOriginCaller),
              (assetDappAddress, Waves)          -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (invokerDappAddress, Waves)        -> (invokerDappBalance, invokerDappAddressWavesBalance)
            )
          )
          checkDataEntriesStateUpdate(actualDataEntries, expectDataEntries, firstTxParticipantAddress.bytes)
        }
      }
    }

    def withAddedBlocksAndSubscribeDoubleNestingInvoke(
        mainDAppTx: SetScriptTransaction,
        nestedDAppTx: SetScriptTransaction,
        doubleNestedDAppTx: SetScriptTransaction
    )(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
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

    def withAddedBlocksAndGetBlockUpdate(
        mainDAppTx: SetScriptTransaction,
        nestedDAppTx: SetScriptTransaction,
        doubleNestedDAppTx: SetScriptTransaction
    )(f: BlockchainUpdated => Unit): Unit = {
      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondAddress, secondAddressBalance),
          AddrWithBalance(invokerDappAddress, invokerDappBalance),
          AddrWithBalance(assetDappAddress, assetDappBalance)
        )
      ) { d =>
        d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
        d.appendBlock(invoke)
      } { getBlockUpdate =>
        f(getBlockUpdate.getUpdate)
      }
    }

    def withAddedBlocksAndGetBlockUpdateRange(
        mainDAppTx: SetScriptTransaction,
        nestedDAppTx: SetScriptTransaction,
        doubleNestedDAppTx: SetScriptTransaction
    )(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 3),
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondAddress, secondAddressBalance),
          AddrWithBalance(invokerDappAddress, invokerDappBalance),
          AddrWithBalance(assetDappAddress, assetDappBalance)
        )
      ) { d =>
        d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
        d.appendBlock(invoke)
        d.appendBlock()
      } { getBlockUpdateRange =>
        f(getBlockUpdateRange)
      }
    }

    def checkInvokeDoubleNestedBlockchainUpdates(
        append: Append,
        transferAddress: Address,
        nestedTransferAddress: Address,
        doubleNestedTransferAddress: Address
    ): Unit = {
      val txMetadata = append.transactionsMetadata.head
      val invokeScript = txMetadata.getInvokeScript
      checkInvokeTransaction(append.transactionIds.head, append.transactionAt(0), invoke, firstTxParticipantAddress.publicKeyHash)
      checkInvokeBaseTransactionMetadata(txMetadata, invoke)
      checkDoubleNestingInvoke(append, invokeScript, issue, transferAddress, nestedTransferAddress, doubleNestedTransferAddress)
    }
  }
}
