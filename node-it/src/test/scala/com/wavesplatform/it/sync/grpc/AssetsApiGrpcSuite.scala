package com.wavesplatform.it.sync.grpc

import com.typesafe.config.Config
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{GrpcIntegrationSuiteWithThreeAddress, NodeConfigs, ReportingTestName}
import org.scalatest.{FreeSpec, Matchers}

class AssetsApiGrpcSuite
    extends FreeSpec
    with Matchers
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with GrpcIntegrationSuiteWithThreeAddress {

  "nftList returns all NFT" in {
    import com.wavesplatform.it.api.SyncGrpcApi._

    val txs = Map(
      "non_nft_asset" -> miner.broadcastIssue(firstAcc, "non_nft_asset", 100, 8, reissuable = true, issueFee + smartFee),
      "nft_asset_1"   -> miner.broadcastIssue(firstAcc, "nft_asset_1", 1, 0, reissuable = false, issueFee + smartFee),
      "nft_asset_2"   -> miner.broadcastIssue(firstAcc, "nft_asset_2", 1, 0, reissuable = false, issueFee + smartFee)
    )

    txs.values.foreach(tx => miner.waitForTransaction(tx.id))

    val allNft = Map(
      "nft_asset_1" -> txs("nft_asset_1").id,
      "nft_asset_2" -> txs("nft_asset_2").id
    )

    val nftList = miner.nftList(firstAddress, 10)
    nftList should have size 2
    nftList.map(_.assetInfo.get.name) should contain theSameElementsAs allNft.keySet
  }

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .buildNonConflicting()
}