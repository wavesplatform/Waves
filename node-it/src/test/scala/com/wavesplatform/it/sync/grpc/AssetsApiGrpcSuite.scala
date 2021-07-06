package com.wavesplatform.it.sync.grpc

import com.typesafe.config.Config
import com.wavesplatform.it._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest

class AssetsApiGrpcSuite
    extends BaseFreeSpec
    with ActivationStatusRequest
    with GrpcIntegrationSuiteWithThreeAddress {

  "nftList returns all NFT" in {
    import com.wavesplatform.it.api.SyncGrpcApi._

    val txs = Map(
      "non_nft_asset" -> sender.broadcastIssue(firstAcc, "non_nft_asset", 100, 8, reissuable = true, issueFee + smartFee),
      "nft_asset_1"   -> sender.broadcastIssue(firstAcc, "nft_asset_1", 1, 0, reissuable = false, issueFee + smartFee),
      "nft_asset_2"   -> sender.broadcastIssue(firstAcc, "nft_asset_2", 1, 0, reissuable = false, issueFee + smartFee)
    )

    txs.values.foreach(tx => sender.waitForTransaction(tx.id))

    val allNft = Map(
      "nft_asset_1" -> txs("nft_asset_1").id,
      "nft_asset_2" -> txs("nft_asset_2").id
    )

    val nftList = sender.nftList(firstAddress, 10)
    nftList should have size 2
    nftList.map(_.assetInfo.get.name) should contain theSameElementsAs allNft.keySet
  }

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .buildNonConflicting()
}