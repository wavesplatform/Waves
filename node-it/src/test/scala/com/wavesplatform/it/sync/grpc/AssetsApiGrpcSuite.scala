package com.wavesplatform.it.sync.grpc

import com.typesafe.config.Config
import com.wavesplatform.it.*
import com.wavesplatform.it.api.SyncGrpcApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.sync.activation.ActivationStatusRequest

class AssetsApiGrpcSuite extends BaseFreeSpec with ActivationStatusRequest with GrpcIntegrationSuiteWithThreeAddress {
  "nftList returns all NFT" in {
    val nftTxs1 =
      (1 to 10)
        .map(i => s"nft_asset_$i" -> sender.broadcastIssue(firstAcc, s"nft_asset_$i", 1, 0, reissuable = false, issueFee + smartFee))
        .toMap

    sender.waitForHeightArise()

    val nftTxs2 =
      (11 to 20)
        .map(i => s"nft_asset_$i" -> sender.broadcastIssue(firstAcc, s"nft_asset_$i", 1, 0, reissuable = false, issueFee + smartFee))
        .toMap

    val nonNftTx = sender.broadcastIssue(firstAcc, "non_nft_asset", 100, 8, reissuable = true, issueFee + smartFee)

    val nftTxs = nftTxs1 ++ nftTxs2
    val nftTxsHeights =
      nftTxs.map { case (_, tx) =>
        sender.waitForTransaction(tx.id)
        sender.getStatus(tx.id).height
      }
    sender.waitForTransaction(nonNftTx.id)

    val nftList = sender.nftList(firstAddress, 20)
    nftList should have size 20
    nftList.map(_.assetInfo.get.name) should contain theSameElementsAs nftTxs.keySet

    val nftHeightsWithSequences = nftList.map(r => (r.assetInfo.get.issueHeight, r.assetInfo.get.sequenceInBlock))
    nftHeightsWithSequences should be(sorted)
    nftHeightsWithSequences.map(_._1) should contain theSameElementsAs nftTxsHeights
    nftHeightsWithSequences.groupMap(_._1)(_._2).values.foreach(nums => nums shouldBe (1 to nums.size))
  }

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .buildNonConflicting()
}
