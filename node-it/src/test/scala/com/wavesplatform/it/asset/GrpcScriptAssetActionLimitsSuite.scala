package com.wavesplatform.it.asset

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi.*
import com.wavesplatform.it.sync.grpc.GrpcBaseTransactionSuiteLike
import com.wavesplatform.it.sync.{issueFee, minFee, smartMinFee}
import com.wavesplatform.lang.directives.values.{V4, V5, V6}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class GrpcScriptAssetActionLimitsSuite extends ScriptAssetActionLimitsSuite with GrpcBaseTransactionSuiteLike {

  def createDApp(script: String, address: KeyPair = miner.generateKeyPair()): KeyPair = {
    val compiledScript = ScriptCompiler
      .compile(
        script,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1

    miner.broadcastTransfer(sender.keyPair, PBRecipients.create(address.toAddress), initialWavesBalance, minFee, waitForTx = true)

    nodes.foreach(
      _.waitForTxAndHeightArise(
        miner.setScript(address, Right(Some(compiledScript)), fee = 1.waves, timestamp = System.currentTimeMillis(), waitForTx = true).id
      )
    )

    address
  }

  Seq(V4, V5, V6).foreach { version =>
    val actionsLimit =
      if (version == V6)
        ContractLimits.MaxAssetScriptActionsAmountV6
      else
        ContractLimits.MaxCallableActionsAmountBeforeV6(version)

    val errMsg =
      if (version == V6)
        "Issue, Reissue, Burn, SponsorFee actions count limit is exceeded"
      else
        "Actions count limit is exceeded"

    s"SponsorFee actions count limit for V${version.id}" in {
      val dApp        = createSponsorFeeDApp(actionsLimit, version)
      val dAppAddress = byteStringAddress(dApp)
      val invokeTx1 = miner.broadcastInvokeScript(
        miner.keyPair,
        PBRecipients.create(dApp.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User(s"issue${actionsLimit}assets"), List.empty)),
        waitForTx = true,
        fee = smartMinFee + issueFee * actionsLimit
      )
      val invokeTx2 = miner.broadcastInvokeScript(
        miner.keyPair,
        PBRecipients.create(dApp.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User(s"sponsor${actionsLimit}assets"), List.empty)),
        waitForTx = true,
        fee = smartMinFee
      )

      val assetIds    = miner.stateChanges(invokeTx1.id)._2.issues.map(_.assetId)
      val sponsorFees = miner.stateChanges(invokeTx2.id)._2.sponsorFees

      (assetIds zip sponsorFees)
        .foreach { case (issueAssetId, sponsorFee) =>
          issueAssetId shouldBe sponsorFee.assetId
          sponsorFee.minSponsoredAssetFee shouldBe Some(minSponsoredAssetFee)

          val assetInfo = miner.assetInfo(issueAssetId)
          assetInfo.sponsorship shouldBe minSponsoredAssetFee
          assetInfo.sponsorBalance shouldBe miner.wavesBalance(dAppAddress).regular

          miner.assetsBalance(dAppAddress).contains(issueAssetId) shouldBe true
        }

      assertGrpcError(
        miner.broadcastInvokeScript(
          miner.keyPair,
          PBRecipients.create(dApp.toAddress),
          Some(FUNCTION_CALL(FunctionHeader.User(s"sponsor${actionsLimit + 1}assets"), List.empty)),
          fee = smartMinFee
        ),
        errMsg
      )
    }

    s"Issue $actionsLimit assets should not produce an error for V${version.id}" in {
      val acc = createDApp(script(actionsLimit, version))
      val tx = miner.broadcastInvokeScript(
        acc,
        PBRecipients.create(acc.toAddress),
        Some(FUNCTION_CALL(FunctionHeader.User(s"issue${actionsLimit}Assets"), List.empty)),
        fee = smartMinFee + issueFee * actionsLimit,
        waitForTx = true
      )
      for (nth <- 0 until actionsLimit) {
        val assetInfo = sender.stateChanges(tx.id)._2.issues(nth)
        assetInfo.quantity shouldBe asset.quantity
        assetInfo.name shouldBe asset.name
        assetInfo.description shouldBe asset.description
        assetInfo.decimals shouldBe asset.decimals
        assetInfo.isReissuable shouldBe asset.reissuable
        sender.assertAssetBalance(acc.toAddress.toString, assetInfo.assetId, asset.quantity)
      }
    }

    s"More than $actionsLimit Issue/Reissue/Burn/SponsorFee actions should produce an error for V${version.id}" in {
      val acc     = createDApp(script(actionsLimit, version))
      val assetId = miner.broadcastIssue(acc, "test", 100, 8, reissuable = true, fee = issueFee, waitForTx = true).id

      assertGrpcError(
        miner.broadcastInvokeScript(
          acc,
          PBRecipients.create(acc.toAddress),
          Some(
            FUNCTION_CALL(
              FunctionHeader.User(s"process${actionsLimit + 1}actions"),
              List(CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet())
            )
          ),
          fee = smartMinFee + issueFee * (actionsLimit + 1),
          waitForTx = true
        ),
        errMsg
      )
    }

    s"More than $actionsLimit issue actions should produce an error for V${version.id}" in {
      val acc = createDApp(script(actionsLimit, version))
      assertGrpcError(
        miner.broadcastInvokeScript(
          acc,
          PBRecipients.create(acc.toAddress),
          Some(FUNCTION_CALL(FunctionHeader.User(s"issue${actionsLimit + 1}Assets"), List.empty)),
          fee = smartMinFee + issueFee * (actionsLimit + 1),
          waitForTx = true
        ),
        errMsg
      )
    }
  }

  private def byteStringAddress(account: KeyPair): ByteString =
    PBRecipients.create(account.toAddress).getPublicKeyHash
}
