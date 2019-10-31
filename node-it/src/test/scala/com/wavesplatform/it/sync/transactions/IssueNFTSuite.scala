package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.it.{Node, NodeConfigs}
import org.scalatest.prop.TableDrivenPropertyChecks

class IssueNFTSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  val firstNode: Node  = nodes.head
  val secondNode: Node = nodes.last

  val secondNodeIssuer = KeyPair("second_node_issuer".getBytes("UTF-8"))
  val firstNodeIssuer  = KeyPair("first_node_issuer".getBytes("UTF-8"))

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.raw(
        """waves {
          |  miner.quorum = 0
          |  blockchain.custom.functionality.pre-activated-features.13 = 10
          |}""".stripMargin))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  test("Can't issue NFT before activation") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    firstNode.transfer(
      firstNode.privateKey.stringRepr,
      firstNodeIssuer.stringRepr,
      10.waves,
      0.001.waves,
      waitForTx = true
    )

    assertApiErrorRaised(
      firstNode.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true)
    )
  }

  test("Able to issue NFT token with reduced fee") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    nodes.waitForHeight(10)

    val nftIssueTxId = secondNode
      .issue(secondNode.address,
        assetName,
        assetDescription,
        quantity = 1,
        decimals = 0,
        reissuable = false,
        fee = 0.001.waves,
        script = None,
        waitForTx = true)
      .id

    nodes.waitForHeightArise()

    secondNode.assertAssetBalance(secondNode.address, nftIssueTxId, 1L)
  }

  test("Can't issue reissuable NFT") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    assertBadRequestAndResponse(
      secondNode.issue(secondNode.address,
        assetName,
        assetDescription,
        quantity = 1,
        decimals = 0,
        reissuable = true,
        fee = 0.001.waves,
        script = None,
        waitForTx = true),
      "does not exceed minimal value"
    )
  }

  test("Can't issue NFT with quantity > 1") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    assertBadRequestAndResponse(
      secondNode.issue(secondNode.address,
        assetName,
        assetDescription,
        quantity = 2,
        decimals = 0,
        reissuable = false,
        fee = 0.001.waves,
        script = None,
        waitForTx = true),
      "does not exceed minimal value"
    )
  }

  test("Can't issue token with reduced fee if decimals > 0") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    assertBadRequestAndResponse(
      secondNode.issue(secondNode.address,
        assetName,
        assetDescription,
        quantity = 1,
        decimals = 1,
        reissuable = false,
        fee = 0.001.waves,
        script = None,
        waitForTx = true),
      "does not exceed minimal value"
    )
  }
  test("nft assets balance should be returned by separate api endpoint") {
    secondNode
      .issue(secondNode.address, "Common", "Common asset", quantity = 1, decimals = 1, reissuable = false, fee = 1.waves, script = None)
      .id
    val issetsId = issueManyAssets(20)
    secondNode.waitForTransaction(issetsId.last)
    nodes.waitForHeightArise()
    val assetsBalance    = secondNode.assetsBalance(secondNode.address).balances.map(a => a.assetId)

    val nftAssetsBalance = secondNode.nftAssetsBalance(secondNode.address, 10).map(id => id.assetId)

    assetsBalance shouldNot contain atLeastOneElementOf nftAssetsBalance
    nftAssetsBalance shouldNot contain atLeastOneElementOf assetsBalance
    nftAssetsBalance.length shouldBe 10

    val remaingNftAssets = secondNode.nftAssetsBalance(secondNode.address, 15, after = nftAssetsBalance.last).map(id => id.assetId)
    remaingNftAssets.length shouldBe 11 // 11 because we issue 1 more in previous test
    remaingNftAssets shouldNot contain atLeastOneElementOf nftAssetsBalance

    val allNFTAssets = secondNode.nftAssetsBalance(secondNode.address, 100).map(id => id.assetId)
    allNFTAssets.length shouldBe 21 // 21 because we issue 1 more in previous test
    allNFTAssets shouldBe nftAssetsBalance ++ remaingNftAssets

  }

  private def issueManyAssets(n: Int): Seq[String] = {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    (1 to n).map(
      i =>
        secondNode
          .issue(secondNode.address,
            assetName + i,
            assetDescription + i,
            quantity = 1,
            decimals = 0,
            reissuable = false,
            fee = 0.001.waves,
            script = None)
          .id)
  }
}
