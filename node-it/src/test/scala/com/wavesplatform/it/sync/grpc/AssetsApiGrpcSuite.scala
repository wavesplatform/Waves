package com.wavesplatform.it.sync.grpc

import com.typesafe.config.Config
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{GrpcIntegrationSuiteWithThreeAddress, NodeConfigs, ReportingTestName}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.Recipient
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{FreeSpec, Matchers}

class AssetsApiGrpcSuite
    extends FreeSpec
    with Matchers
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with GrpcIntegrationSuiteWithThreeAddress {
  import AssetsApiGrpcSuite._

  "nftList returns all NFT" in {
    import com.wavesplatform.it.api.SyncGrpcApi._

    sender.setScript(firstAcc, Right(Some(script)), setScriptFee, waitForTx = true)

    val txs = Map(
      "non_nft_asset" -> sender.broadcastIssue(firstAcc, "non_nft_asset", 100, 8, reissuable = true, issueFee + smartFee),
      "nft_asset_1"   -> sender.broadcastIssue(firstAcc, "nft_asset_1", 1, 0, reissuable = false, issueFee + smartFee),
      "nft_asset_2"   -> sender.broadcastIssue(firstAcc, "nft_asset_2", 1, 0, reissuable = false, issueFee + smartFee),
      "nft_asset_3" -> sender.broadcastInvokeScript(
        firstAcc,
        Recipient().withPublicKeyHash(firstAddress),
        Some(FUNCTION_CALL(FunctionHeader.User("nft"), List.empty)),
        fee = invokeFee + 2 * smartFee
      )
    )

    txs.values.foreach(tx => sender.waitForTransaction(tx.id))

    val allNft = Map(
      "nft_asset_1" -> txs("nft_asset_1").id,
      "nft_asset_2" -> txs("nft_asset_2").id,
      "nft_asset_3" -> sender.stateChanges(txs("nft_asset_3").id)._2.issues.head.assetId
    )

    val nftList = sender.nftList(firstAddress)
    nftList should have size 3
    nftList.map(_.name) should contain theSameElementsAs allNft.keySet
  }

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .buildNonConflicting()
}

object AssetsApiGrpcSuite {
  private val script = ScriptCompiler
    .compile(
      """
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |@Callable(inv)
        |func nft() = {
        |  [Issue("nft_asset_3", "", 1, 0, false, unit, 0)]
        |}
        |""".stripMargin,
      ScriptEstimatorV3
    )
    .explicitGet()
    ._1
}
