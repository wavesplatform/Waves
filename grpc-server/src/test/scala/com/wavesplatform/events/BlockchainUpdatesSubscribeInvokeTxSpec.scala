package com.wavesplatform.events

import akka.http.scaladsl.marshalling.ToResponseMarshallable.apply
import com.wavesplatform.TestValues.fee
import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.fixtures.PrepareInvokeTestData
import com.wavesplatform.events.fixtures.PrepareInvokeTestData.{invokeAssetScript, issueAssetAmount, scriptTransferIssueAssetInt}
import com.wavesplatform.events.fixtures.WavesTxChecks.{
  checkBalances,
  checkInvokeBaseTransactionMetadata,
  checkInvokeResultTransactionMetadata,
  checkInvokeTransaction
}
import com.wavesplatform.events.protobuf.StateUpdate.BalanceUpdate
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, EXPR}
import com.wavesplatform.protobuf.transaction.PBAmounts.toVanillaAssetId
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.assets.IssueTransaction
import org.scalactic.source.Position
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeInvokeTxSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6

  val dAppAccount: SeedKeyPair  = TxHelpers.signer(12)
  val dAppAddress: Address      = dAppAccount.toAddress
  var dAppBalanceBefore: Long   = 10.waves
  val sender: SeedKeyPair       = TxHelpers.signer(2436)
  val senderAddress: Address    = sender.toAddress
  val senderBalanceBefore: Long = 10.waves

  "BU-31. Return correct data for Invoke" in {
    val issue                         = TxHelpers.issue(dAppAccount)
    val asset                         = issue.asset
    val assetByteStr                  = CONST_BYTESTR(asset.id).explicitGet()
    val addressByteStr                = CONST_BYTESTR(ByteStr.apply(senderAddress.bytes)).explicitGet()
    val args: Seq[EXPR]               = Seq(assetByteStr, addressByteStr)
    val invoke                        = TxHelpers.invoke(dAppAddress, Some("setData"), args, Seq.empty, sender, fee = 100500000L)
    val data                          = PrepareInvokeTestData.dataMap
    var dAppBalanceAfterTx            = 0L
    val dAppInvokeIssueBalance: Long  = issueAssetAmount - scriptTransferIssueAssetInt
    var dAppAssetBalanceAfterTx: Long = 0L
    var senderBalanceAfterTx          = 0L

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
      val append              = updates(3).append
      val transactionMetadata = append.transactionsMetadata
      val result              = transactionMetadata.head.getInvokeScript.result
      val invokeIssueAsset    = toVanillaAssetId(result.get.issues.head.assetId)

      checkInvokeTransaction(append.transactionIds.head, append.transactionAt(0), invoke, dAppAddress.publicKeyHash)
      checkInvokeBaseTransactionMetadata(transactionMetadata, invoke, asset.id, senderAddress.bytes)
      checkInvokeResultTransactionMetadata(result, data, asset.id, senderAddress)
      checkBalances(
        append.transactionStateUpdates.head.balances,
        Map(
          (senderAddress, Waves)            -> (senderBalanceBefore, senderBalanceAfterTx),
          (senderAddress, asset)            -> (0, data.apply("scriptTransferAssetInt").toString.toLong),
          (senderAddress, invokeIssueAsset) -> (0, data.apply("scriptTransferIssueAssetInt").toString.toLong),
          (dAppAddress, Waves)              -> (dAppBalanceBefore, dAppBalanceAfterTx),
          (dAppAddress, invokeIssueAsset)   -> (0, dAppInvokeIssueBalance),
          (dAppAddress, asset)              -> (issue.quantity.value, dAppAssetBalanceAfterTx)
        )
      )

    }
  }
}
