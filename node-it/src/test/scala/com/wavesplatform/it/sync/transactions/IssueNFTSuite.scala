package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.it.{Node, NodeConfigs}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import org.scalatest.prop.TableDrivenPropertyChecks

class IssueNFTSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  val firstNode: Node  = nodes.head
  val secondNode: Node = nodes.last

  val secondNodeIssuer = KeyPair("second_node_issuer".getBytes())
  val firstNodeIssuer  = KeyPair("first_node_issuer".getBytes())

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(_.raw(s"""
           |waves.blockchain.custom.functionality.pre-activated-features = {
           |          2 = 0
           |          3 = 0
           |          4 = 0
           |          5 = 0
           |          6 = 0
           |          7 = 0
           |          9 = 0
           |          10 = 0
           |          11 = 0
           |          12 = 0
           |          13 = 0
           |}
         """.stripMargin))
      .buildNonConflicting()

  test("Can't issue NFT before activation") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    firstNode.transfer(
      firstNode.privateKey.address,
      firstNodeIssuer.address,
      10.waves,
      0.001.waves,
      waitForTx = true
    )

    assertBadRequest(
      firstNode.issue(firstAddress, assetName, assetDescription, 1, 0, reissuable = false, 1.waves / 1000, waitForTx = true)
    )
  }

  test("Able to issue NFT token with reduced fee") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    val ttx = TransferTransactionV1
      .selfSigned(
        Waves,
        secondNode.privateKey,
        secondNodeIssuer,
        10.waves,
        System.currentTimeMillis(),
        Waves,
        0.001.waves,
        Array.emptyByteArray
      )
      .explicitGet()

    secondNode.signedBroadcast(ttx.json(), waitForTx = true)

    val itx = IssueTransactionV2
      .selfSigned(
        'I',
        secondNodeIssuer,
        assetName.getBytes(),
        assetDescription.getBytes(),
        1,
        0,
        false,
        None,
        0.001.waves,
        System.currentTimeMillis()
      )
      .explicitGet()

    secondNode.signedBroadcast(itx.json(), waitForTx = true)

    secondNode.assertAssetBalance(secondNodeIssuer.address, itx.assetId().toString, 1L)
  }

  test("Can't issue reissuable NFT") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    val itx = IssueTransactionV2
      .selfSigned(
        'I',
        secondNodeIssuer,
        assetName.getBytes(),
        assetDescription.getBytes(),
        1,
        0,
        true,
        None,
        0.001.waves,
        System.currentTimeMillis()
      )
      .explicitGet()

    assertBadRequestAndResponse(secondNode.signedBroadcast(itx.json(), waitForTx = true), "does not exceed minimal value")
  }

  test("Can't issue NFT with quantity > 1") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    val itx = IssueTransactionV2
      .selfSigned(
        'I',
        secondNodeIssuer,
        assetName.getBytes(),
        assetDescription.getBytes(),
        2,
        0,
        true,
        None,
        0.001.waves,
        System.currentTimeMillis()
      )
      .explicitGet()

    assertBadRequestAndResponse(secondNode.signedBroadcast(itx.json(), waitForTx = true), "does not exceed minimal value")
  }

  test("Can't issue token with reduced fee if decimals > 0") {
    val assetName        = "NFTAsset"
    val assetDescription = "my asset description"

    val itx = IssueTransactionV2
      .selfSigned(
        'I',
        secondNodeIssuer,
        assetName.getBytes(),
        assetDescription.getBytes(),
        1,
        1,
        true,
        None,
        0.001.waves,
        System.currentTimeMillis()
      )
      .explicitGet()

    assertBadRequestAndResponse(secondNode.signedBroadcast(itx.json(), waitForTx = true), "does not exceed minimal value")
  }
}
