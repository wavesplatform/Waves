package com.wavesplatform.events.fixtures

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.BlockchainUpdatesTestBase
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdatesRangeRequest, SubscribeRequest}
import com.wavesplatform.events.fixtures.BlockchainUpdateGrpcMethod.*
import com.wavesplatform.events.fixtures.InvokeWavesTxCheckers.checkSimpleInvoke
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.*
import com.wavesplatform.events.fixtures.WavesTxChecks.checkBalances
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.events.protobuf.TransactionMetadata.InvokeScriptMetadata
import com.wavesplatform.transaction.{Asset, Transaction, TxHelpers}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.transaction.TxHelpers.secondSigner

object TestHelpers extends BlockchainUpdatesTestBase {
  def testInvoke(issue: IssueTransaction, invoke: Transaction, balances: Seq[AddrWithBalance])(
    checkType: BlockchainUpdateGrpcMethod,
    checkFunction: Append => Unit
  ): Unit = {
    for (libVersion <- 5 to 6) {
      val setScript = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(invokeAssetScript(libVersion)))
      checkType match {
        case Subscribe =>
          withGenerateSubscription(
            settings = currentSettings,
            balances = balances
          ) { d =>
            d.appendBlock(setScript)
            d.appendBlock(issue)
            d.appendMicroBlock(invoke)
          } { updates =>
            checkFunction(updates(3).append)
          }
        case GetBlockUpdate =>
          withGenerateGetBlockUpdate(
            height = 4,
            settings = currentSettings,
            balances
          ) { d =>
            d.appendBlock(setScript)
            d.appendBlock(issue)
            d.appendBlock(invoke)
          } { getBlockUpdate =>
            checkFunction(getBlockUpdate.getUpdate.getAppend)
          }
        case GetBlockUpdateRange =>
          withGenerateGetBlockUpdateRange(
            GetBlockUpdatesRangeRequest.of(1, 4),
            settings = currentSettings,
            balances = balances
          ) { d =>
            d.appendBlock(setScript)
            d.appendBlock(issue)
            d.appendBlock(invoke)
            d.appendBlock()
          } { getBlockUpdateRange =>
            checkFunction(getBlockUpdateRange.apply(3).getAppend)
          }
        case _ => throw new Exception("handle unsupported checkType")
      }
    }
  }

  def doubleNestedInvokeTest(
      assetDappAccount: SeedKeyPair,
      balances: Seq[AddrWithBalance],
      issue: IssueTransaction,
      invoke: Transaction,
      massTx: Transaction,
      callerType: String,
      checkType: BlockchainUpdateGrpcMethod
  )(f: Append => Unit): Unit = {
    for (libVersion <- 5 to 6) {
      val mainDAppTx         = TxHelpers.setScript(firstTxParticipant, TxHelpers.script(mainDAppScript(libVersion)))
      val nestedDAppTx       = TxHelpers.setScript(secondSigner, TxHelpers.script(nestedDAppScript(callerType, libVersion)))
      val doubleNestedDAppTx = TxHelpers.setScript(assetDappAccount, TxHelpers.script(doubleNestedDAppScript(callerType, libVersion)))

      checkType match {
        case Subscribe =>
          withGenerateSubscription(
            SubscribeRequest.of(1, Int.MaxValue),
            settings = currentSettings,
            balances
          ) { d =>
            d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
            d.appendMicroBlock(invoke)
          } { updates =>
            f(updates(2).getAppend)
          }

        case GetBlockUpdate =>
          withGenerateGetBlockUpdate(
            height = 3,
            settings = currentSettings,
            balances
          ) { d =>
            d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
            d.appendBlock(invoke)
          } { getBlockUpdate =>
            f(getBlockUpdate.getUpdate.getAppend)
          }

        case GetBlockUpdateRange =>
          withGenerateGetBlockUpdateRange(
            GetBlockUpdatesRangeRequest.of(1, 3),
            settings = currentSettings,
            balances
          ) { d =>
            d.appendBlock(issue, massTx, mainDAppTx, nestedDAppTx, doubleNestedDAppTx)
            d.appendBlock(invoke)
            d.appendBlock()
          } { getBlockUpdateRange =>
            f(getBlockUpdateRange.apply(2).getAppend)
          }
      }
    }
  }

  def checkGeneralInvoke(
      append: Append,
      issuerAssetBalanceAfterTx: Long,
      invoke: Transaction,
      issue: IssueTransaction,
      invokeScript: InvokeScriptMetadata,
      expectBalancesMap: Map[(Address, Asset), (Long, Long)]
  ): Unit = {
    val issuerInvokeIssueBalance: Long = issueData.apply("amount").toString.toLong - scriptTransferIssueAssetNum
    val result                         = invokeScript.result.get
    val invokeIssueAsset               = toVanillaAssetId(result.issues.head.assetId)
    val expectMap = Map(
      (firstTxParticipantAddress, invokeIssueAsset)  -> (0L, issuerInvokeIssueBalance),
      (secondTxParticipantAddress, invokeIssueAsset) -> (0L, scriptTransferIssueAssetNum)
    ) ++ expectBalancesMap
    checkSimpleInvoke(append, issue, issuerAssetBalanceAfterTx, invokeScript, invoke.id.value().arr)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      expectMap
    )
  }
}
