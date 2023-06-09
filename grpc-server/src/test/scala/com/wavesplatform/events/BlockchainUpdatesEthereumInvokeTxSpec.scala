package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.api.grpc.protobuf.GetBlockUpdatesRangeRequest
import com.wavesplatform.events.fixtures.InvokeWavesTxCheckers.{checkingDoubleNestingInvoke, checkingSimpleInvoke}
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.*
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.EthTxGenerator.Arg
import com.wavesplatform.transaction.{EthTxGenerator, EthereumTransaction, TxHelpers, TxNonNegativeAmount}
import com.wavesplatform.transaction.TxHelpers.{secondAddress, secondSigner}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer

class BlockchainUpdatesEthereumInvokeTxSpec extends BlockchainUpdatesTestBase {
  val ethAddressBalanceAfterTx: Long = firstTxParticipantBalanceBefore - invokeFee

  "Simple invoke transaction" - {
    val issue          = TxHelpers.issue(firstTxParticipant)
    val asset          = issue.asset
    val assetByteStr   = Arg.Bytes(asset.id)
    val addressByteStr = Arg.Bytes(ByteStr.apply(secondTxParticipantAddress.bytes))
    val args: Seq[Arg] = Seq(assetByteStr, addressByteStr)
    val invoke: EthereumTransaction =
      EthTxGenerator.generateEthInvoke(firstTxParticipantEthereum, firstTxParticipantAddress, invokeFunctionName, args, Seq.empty, invokeFee)
    val issuerInvokeIssueBalance: Long         = issueData.apply("amount").toString.toLong - scriptTransferIssueAssetNum
    val issuerAssetBalanceAfterTx: Long        = issue.quantity.value - burnNum - scriptTransferAssetNum + reissueNum
    val secondAddressWavesBalanceAfterTx: Long = secondTxParticipantBalanceBefore + scriptTransferUnitNum

    "BU-226. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val setScript                       = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
        val issuerBalanceBeforeInvoke: Long = firstTxParticipantBalanceBefore - issue.fee.value - setScript.fee.value
        withGenerateSubscription(
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
          val append = updates(3).append
          checkingEthInvoke(append, issuerBalanceBeforeInvoke)
        }
      }
    }

    "BU-229. Invoke have to return correct data for getBlockUpdate" in {
      for (libVersion <- 5 to 6) {
        val setScript                       = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
        val issuerBalanceBeforeInvoke: Long = firstTxParticipantBalanceBefore - issue.fee.value - setScript.fee.value
        withGenerateGetBlockUpdate(
          height = 4,
          settings = currentSettings,
          balances = Seq(
            AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(firstTxParticipantEthereumAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
          )
        ) { d =>
          d.appendBlock(setScript)
          d.appendBlock(issue)
          d.appendBlock(invoke)
        } { getBlockUpdate =>
          val append = getBlockUpdate.getUpdate.getAppend
          checkingEthInvoke(append, issuerBalanceBeforeInvoke)
        }
      }
    }

    "BU-232. Invoke have to return correct data for getBlockUpdateRange" in {
      for (libVersion <- 5 to 6) {
        val setScript                       = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
        val issuerBalanceBeforeInvoke: Long = firstTxParticipantBalanceBefore - issue.fee.value - setScript.fee.value
        withGenerateGetBlockUpdateRange(
          GetBlockUpdatesRangeRequest.of(1, 4),
          settings = currentSettings,
          balances = Seq(
            AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(firstTxParticipantEthereumAddress, firstTxParticipantBalanceBefore),
            AddrWithBalance(secondTxParticipantAddress, secondTxParticipantBalanceBefore)
          )
        ) { d =>
          d.appendBlock(setScript)
          d.appendBlock(issue)
          d.appendBlock(invoke)
          d.appendBlock()
        } { getBlockUpdateRange =>
          val append = getBlockUpdateRange.apply(3).getAppend
          checkingEthInvoke(append, issuerBalanceBeforeInvoke)
        }
      }
    }

    def checkingEthInvoke(append: Append, issuerBalanceBeforeInvoke: Long): Unit = {
      val transactionMetadata = append.transactionsMetadata
      val invokeScript        = transactionMetadata.head.getEthereum.getInvoke
      val result              = invokeScript.result.get
      val invokeIssueAsset    = toVanillaAssetId(result.issues.head.assetId)
      val invokeLeaseId       = result.leases.head.leaseId.toByteArray
      checkEthereumTransaction(append.transactionIds.head, append.transactionAt(0), invoke)
      checkEthereumInvokeBaseTransactionMetadata(transactionMetadata, invoke, invokeFunctionName, firstTxParticipantAddress)
      checkingSimpleInvoke(append, issue, issuerAssetBalanceAfterTx, invokeScript)
      checkBalances(
        append.transactionStateUpdates.head.balances,
        Map(
          (firstTxParticipantEthereumAddress, Waves)     -> (firstTxParticipantBalanceBefore, ethAddressBalanceAfterTx),
          (firstTxParticipantAddress, Waves)             -> (issuerBalanceBeforeInvoke, issuerBalanceBeforeInvoke - scriptTransferUnitNum),
          (secondTxParticipantAddress, Waves)            -> (secondTxParticipantBalanceBefore, secondAddressWavesBalanceAfterTx),
          (firstTxParticipantAddress, asset)             -> (issue.quantity.value, issuerAssetBalanceAfterTx),
          (firstTxParticipantAddress, invokeIssueAsset)  -> (0, issuerInvokeIssueBalance),
          (secondTxParticipantAddress, invokeIssueAsset) -> (0, scriptTransferIssueAssetNum),
          (secondTxParticipantAddress, asset)            -> (0, scriptTransferAssetNum)
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
    val args: Seq[Arg] =
      Seq(
        Arg.Bytes(ByteStr.apply(secondAddress.bytes)),
        Arg.Bytes(ByteStr.apply(assetDappAddress.bytes)),
        Arg.Integer(scriptTransferUnitNum),
        Arg.Str(bar),
        Arg.Bytes(asset.id)
      )
    val invoke = EthTxGenerator.generateEthInvoke(firstTxParticipantEthereum, firstTxParticipantAddress, "foo", args, Seq.empty, invokeFee)
    val secondAddressBalance: Long       = 8.waves
    val assetDappBalance: Long           = 12.waves
    val secondAddressAssetBalanceForAll  = amount - scriptTransferAssetNum + paymentNum
    val dAppAddressAssetBalanceForCaller = amount + scriptTransferAssetNum - paymentNum
    val dAppAddressAssetBalanceForOriginalCaller = amount - paymentNum


    "BU-227. doubles nested i.caller. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx                      = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx                    = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.caller", libVersion)))
        val doubleNestedDAppTx              = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.caller", libVersion)))
        val secondAddressWavesBalanceBefore = secondAddressBalance - nestedDAppTx.fee.value
        val secondAddressWavesBalanceAfter  = secondAddressWavesBalanceBefore + scriptTransferUnitNum
        val assetDappAddressWavesBalanceBefore = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter  = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum

        addedBlocksAndSubscribeDoubleNestingInvoke(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val balances = updates(2).getAppend.transactionStateUpdates.head.balances
          checkInvokeDoubleNestedBlockchainUpdates(updates(2).getAppend, assetDappAddress, firstTxParticipantAddress, secondAddress)
          checkBalances(
            balances,
            Map(
              (secondAddress, Waves)                     -> (secondAddressWavesBalanceBefore, secondAddressWavesBalanceAfter),
              (secondAddress, asset)                     -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset)         -> (amount, dAppAddressAssetBalanceForCaller),
              (assetDappAddress, Waves)                  -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (firstTxParticipantEthereumAddress, Waves) -> (firstTxParticipantBalanceBefore, ethAddressBalanceAfterTx)
            )
          )
        }
      }
    }

    "BU-230. doubles nested i.caller. Invoke have to return correct data for getBlockUpdate" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx                      = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx                    = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.caller", libVersion)))
        val doubleNestedDAppTx              = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.caller", libVersion)))
        val secondAddressWavesBalanceBefore = secondAddressBalance - nestedDAppTx.fee.value
        val secondAddressWavesBalanceAfter  = secondAddressWavesBalanceBefore + scriptTransferUnitNum
        val assetDappAddressWavesBalanceBefore = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter  = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum

        addedBlocksAndGetBlockUpdate(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { update =>
          val balances = update.getAppend.transactionStateUpdates.head.balances
          checkInvokeDoubleNestedBlockchainUpdates(update.getAppend, assetDappAddress, firstTxParticipantAddress, secondAddress)
          checkBalances(
            balances,
            Map(
              (secondAddress, Waves)                     -> (secondAddressWavesBalanceBefore, secondAddressWavesBalanceAfter),
              (secondAddress, asset)                     -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset)         -> (amount, dAppAddressAssetBalanceForCaller),
              (assetDappAddress, Waves)                  -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (firstTxParticipantEthereumAddress, Waves) -> (firstTxParticipantBalanceBefore, ethAddressBalanceAfterTx)
            )
          )
        }
      }
    }

    "BU-233. doubles nested i.caller. Invoke have to return correct data for getBlockUpdateRange" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx                      = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx                    = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.caller", libVersion)))
        val doubleNestedDAppTx              = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.caller", libVersion)))
        val secondAddressWavesBalanceBefore = secondAddressBalance - nestedDAppTx.fee.value
        val secondAddressWavesBalanceAfter  = secondAddressWavesBalanceBefore + scriptTransferUnitNum
        val assetDappAddressWavesBalanceBefore = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter  = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum

        addedBlocksAndGetBlockUpdateRange(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val balances = updates(2).getAppend.transactionStateUpdates.head.balances
          checkInvokeDoubleNestedBlockchainUpdates(updates(2).getAppend, assetDappAddress, firstTxParticipantAddress, secondAddress)
          checkBalances(
            balances,
            Map(
              (secondAddress, Waves)                     -> (secondAddressWavesBalanceBefore, secondAddressWavesBalanceAfter),
              (secondAddress, asset)                     -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantAddress, asset)         -> (amount, dAppAddressAssetBalanceForCaller),
              (assetDappAddress, Waves)                  -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (firstTxParticipantEthereumAddress, Waves) -> (firstTxParticipantBalanceBefore, ethAddressBalanceAfterTx)
            )
          )
        }
      }
    }

    "BU-228. double nested i.originCaller. Invoke have to return correct data for subscribe" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.originCaller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.originCaller", libVersion)))
        val assetDappAddressWavesBalanceBefore = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter  = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val firstTxParticipantBalanceAfter     = ethAddressBalanceAfterTx + scriptTransferUnitNum

        addedBlocksAndSubscribeDoubleNestingInvoke(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val balances = updates(2).getAppend.transactionStateUpdates.head.balances
          checkInvokeDoubleNestedBlockchainUpdates(
            updates(2).getAppend,
            assetDappAddress,
            firstTxParticipantEthereumAddress,
            firstTxParticipantEthereumAddress
          )
          checkBalances(
            balances,
            Map(
              (secondAddress, asset)                     -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantEthereumAddress, asset) -> (0, scriptTransferAssetNum),
              (firstTxParticipantAddress, asset)         -> (amount, dAppAddressAssetBalanceForOriginalCaller),
              (assetDappAddress, Waves)                  -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (firstTxParticipantEthereumAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter)
            )
          )
        }
      }
    }

    "BU-231. double nested i.originCaller. Invoke have to return correct data for getBlockUpdate" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.originCaller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.originCaller", libVersion)))
        val assetDappAddressWavesBalanceBefore = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter  = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val firstTxParticipantBalanceAfter     = ethAddressBalanceAfterTx + scriptTransferUnitNum

        addedBlocksAndGetBlockUpdate(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { update =>
          val balances = update.getAppend.transactionStateUpdates.head.balances
          checkInvokeDoubleNestedBlockchainUpdates(
            update.getAppend,
            assetDappAddress,
            firstTxParticipantEthereumAddress,
            firstTxParticipantEthereumAddress
          )
          checkBalances(
            balances,
            Map(
              (secondAddress, asset)                     -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantEthereumAddress, asset) -> (0, scriptTransferAssetNum),
              (firstTxParticipantAddress, asset)         -> (amount, dAppAddressAssetBalanceForOriginalCaller),
              (assetDappAddress, Waves)                  -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (firstTxParticipantEthereumAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter)
            )
          )
        }
      }
    }

    "BU-234. double nested i.originCaller. Invoke have to return correct data for getBlockUpdateRange" in {
      for (libVersion <- 5 to 6) {
        val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
        val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript("i.originCaller", libVersion)))
        val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript("i.originCaller", libVersion)))
        val assetDappAddressWavesBalanceBefore = assetDappBalance - issueAssetFee - massTx.fee.value - doubleNestedDAppTx.fee.value
        val assetDappAddressWavesBalanceAfter  = assetDappAddressWavesBalanceBefore - scriptTransferUnitNum
        val firstTxParticipantBalanceAfter     = ethAddressBalanceAfterTx + scriptTransferUnitNum

        addedBlocksAndGetBlockUpdateRange(mainDAppTx, nestedDAppTx, doubleNestedDAppTx) { updates =>
          val balances = updates(2).getAppend.transactionStateUpdates.head.balances
          checkInvokeDoubleNestedBlockchainUpdates(
            updates(2).getAppend,
            assetDappAddress,
            firstTxParticipantEthereumAddress,
            firstTxParticipantEthereumAddress
          )
          checkBalances(
            balances,
            Map(
              (secondAddress, asset)                     -> (amount, secondAddressAssetBalanceForAll),
              (firstTxParticipantEthereumAddress, asset) -> (0, scriptTransferAssetNum),
              (firstTxParticipantAddress, asset)         -> (amount, dAppAddressAssetBalanceForOriginalCaller),
              (assetDappAddress, Waves)                  -> (assetDappAddressWavesBalanceBefore, assetDappAddressWavesBalanceAfter),
              (firstTxParticipantEthereumAddress, Waves) -> (firstTxParticipantBalanceBefore, firstTxParticipantBalanceAfter)
            )
          )
        }
      }
    }

    def checkInvokeDoubleNestedBlockchainUpdates(
        append: Append,
        transferAddress: Address,
        nestedTransferAddress: Address,
        doubleNestedTransferAddress: Address
    ): Unit = {
      val invokeScript = append.transactionsMetadata.head.getEthereum.getInvoke
      val txMetadata   = append.transactionsMetadata
      checkEthereumTransaction(append.transactionIds.head, append.transactionAt(0), invoke)
      checkEthereumInvokeBaseTransactionMetadata(txMetadata, invoke, foo, firstTxParticipantAddress)
      checkingDoubleNestingInvoke(append, invokeScript, issue, transferAddress, nestedTransferAddress, doubleNestedTransferAddress)
    }

    def addedBlocksAndSubscribeDoubleNestingInvoke(
        mainDAppTx: SetScriptTransaction,
        nestedDAppTx: SetScriptTransaction,
        doubleNestedDAppTx: SetScriptTransaction
    )(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
      withGenerateSubscription(
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(firstTxParticipantEthereumAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondAddress, secondAddressBalance),
          AddrWithBalance(invokerDappAddress, ethAddressBalanceAfterTx),
          AddrWithBalance(assetDappAddress, assetDappBalance)
        )
      ) { d =>
        d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
        d.appendMicroBlock(invoke)
      } { updates =>
        f(updates)
      }
    }

    def addedBlocksAndGetBlockUpdate(
        mainDAppTx: SetScriptTransaction,
        nestedDAppTx: SetScriptTransaction,
        doubleNestedDAppTx: SetScriptTransaction
    )(f: PBBlockchainUpdated => Unit): Unit = {
      withGenerateGetBlockUpdate(
        height = 3,
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(firstTxParticipantEthereumAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondAddress, secondAddressBalance),
          AddrWithBalance(invokerDappAddress, ethAddressBalanceAfterTx),
          AddrWithBalance(assetDappAddress, assetDappBalance)
        )
      ) { d =>
        d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
        d.appendBlock(invoke)
      } { getBlockUpdate =>
        f(getBlockUpdate.getUpdate)
      }
    }

    def addedBlocksAndGetBlockUpdateRange(
        mainDAppTx: SetScriptTransaction,
        nestedDAppTx: SetScriptTransaction,
        doubleNestedDAppTx: SetScriptTransaction
    )(f: Seq[PBBlockchainUpdated] => Unit): Unit = {
      withGenerateGetBlockUpdateRange(
        GetBlockUpdatesRangeRequest.of(1, 3),
        settings = currentSettings,
        balances = Seq(
          AddrWithBalance(firstTxParticipantAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(firstTxParticipantEthereumAddress, firstTxParticipantBalanceBefore),
          AddrWithBalance(secondAddress, secondAddressBalance),
          AddrWithBalance(invokerDappAddress, ethAddressBalanceAfterTx),
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
  }
}
