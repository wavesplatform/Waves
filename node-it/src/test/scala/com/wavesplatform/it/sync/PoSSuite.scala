package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.api.http.DebugMessage
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.consensus.FairPoSCalculator
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto
import com.wavesplatform.it.api.AsyncNetworkApi.NodeAsyncNetworkApi
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, WaitForHeight2}
import com.wavesplatform.network.RawBytes
import play.api.libs.json.{JsSuccess, Json, Reads}

import scala.util.Random

class PoSSuite extends BaseFunSuite with WaitForHeight2 {

  private val signerPK = KeyPair.fromSeed(nodeConfigs.last.getString("account-seed")).explicitGet()

  private implicit val nxtCDataReads: Reads[NxtLikeConsensusBlockData] = Reads { json =>
    val bt = (json \ "base-target").as[Long]
    val gs = (json \ "generation-signature").as[String]

    JsSuccess(NxtLikeConsensusBlockData(bt, ByteStr.decodeBase58(gs).get))
  }

  test("BlockV4: Node mines several blocks, integration test checks that block timestamps equal to time of appearence (+-1100ms)") {

    val height = nodes.last.height

    for (h <- height to (height + 10)) {

      val block = forgeBlock(h, signerPK)()

      nodes.waitForHeightArise()

      val newTimestamp = blockTimestamp(h + 1)

      block.header.timestamp shouldBe (newTimestamp +- 1100)
    }
  }

  test("BlockV4: Accept correct block") {
    val height = nodes.head.height

    val block = forgeBlock(height, signerPK)()

    waitForBlockTime(block)

    nodes.head.printDebugMessage(DebugMessage(s"Send block for $height"))
    nodes.head.sendByNetwork(RawBytes.fromBlock(block))

    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig sameElements block.id().arr
  }

  test("BlockV4: Reject block with invalid delay") {
    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)(updateDelay = _ - 1000)

    waitForBlockTime(block)

    nodes.head.sendByNetwork(RawBytes.fromBlock(block))
    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.id().arr
  }

  test("BlockV4: Reject block with invalid BT") {
    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)(updateBaseTarget = _ + 2)

    waitForBlockTime(block)

    nodes.head.sendByNetwork(RawBytes.fromBlock(block))

    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.id().arr
  }

  test("BlockV4: Reject block with invalid generation signature") {
    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)()
    block.copy(header = block.header.copy(generationSignature = {
      val arr  = block.header.generationSignature.arr
      val init = arr.init
      Random.nextBytes(arr)
      ByteStr(init :+ arr.last)
    }))

    waitForBlockTime(block)

    nodes.head.printDebugMessage(DebugMessage(s"Send invalid block for $height"))
    nodes.head.sendByNetwork(RawBytes.fromBlock(block))

    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.id().arr
  }

  test("BlockV4: Reject block with invalid signature") {
    val otherNodePK = KeyPair.fromSeed(nodeConfigs.head.getString("account-seed")).explicitGet()

    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)(updateBaseTarget = _ + 2)

    val signature = crypto.sign(otherNodePK.privateKey, block.bytes())
    val resignedBlock =
      block
        .copy(signature = signature)

    waitForBlockTime(resignedBlock)

    nodes.head.sendByNetwork(RawBytes.fromBlock(resignedBlock))

    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be resignedBlock.id().arr
  }

  test("BlockV5: Node mines several blocks, integration test checks that block timestamps equal to time of appearence (+-1100ms)") {
    nodes.waitForHeight(vrfActivationHeight)
    val height = nodes.last.height

    for (h <- height to (height + 10)) {

      val block = forgeBlock(h, signerPK)()

      nodes.waitForHeightArise()

      val newTimestamp = blockTimestamp(h + 1)

      block.header.timestamp shouldBe (newTimestamp +- 1100)
    }
  }

  test("BlockV5: Accept correct block") {

    nodes.last.close()
    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)()

    waitForBlockTime(block)

    nodes.head.printDebugMessage(DebugMessage(s"Send block for $height"))
    nodes.head.sendByNetwork(RawBytes.fromBlock(block))

    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig sameElements block.id().arr
  }

  test("BlockV5: Reject block with invalid delay") {
    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)(updateDelay = _ - 1000)

    waitForBlockTime(block)

    nodes.head.sendByNetwork(RawBytes.fromBlock(block))
    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.id().arr
  }

  test("BlockV5: Reject block with invalid BT") {
    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)(updateBaseTarget = _ + 2)

    waitForBlockTime(block)

    nodes.head.sendByNetwork(RawBytes.fromBlock(block))

    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.id().arr
  }

  test("BlockV5: Reject block with invalid generation signature") {
    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)()
    block.copy(header = block.header.copy(generationSignature = {
      val arr  = block.header.generationSignature.arr
      val init = arr.init
      Random.nextBytes(arr)
      ByteStr(init :+ arr.last)
    }))

    waitForBlockTime(block)

    nodes.head.printDebugMessage(DebugMessage(s"Send invalid block for $height"))
    nodes.head.sendByNetwork(RawBytes.fromBlock(block))

    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.id().arr
  }

  test("BlockV5: Reject block with invalid signature") {
    val otherNodePK = KeyPair.fromSeed(nodeConfigs.head.getString("account-seed")).explicitGet()

    val height = nodes.head.height
    val block  = forgeBlock(height, signerPK)(updateBaseTarget = _ + 2)

    val signature = crypto.sign(otherNodePK.privateKey, block.bytes())
    val resignedBlock =
      block
        .copy(signature = signature)

    waitForBlockTime(resignedBlock)

    nodes.head.sendByNetwork(RawBytes.fromBlock(resignedBlock))

    nodes.head.waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be resignedBlock.id().arr
  }

  def waitForBlockTime(block: Block): Unit = {
    val timeout = block.header.timestamp - System.currentTimeMillis()

    if (timeout > 0) Thread.sleep(timeout)
  }

  def blockInfo(height: Int): (Array[Byte], Long, NxtLikeConsensusBlockData, Option[ByteStr]) = {
    val lastBlock      = Json.parse(nodes.head.get(s"/blocks/at/$height").getResponseBody)
    val lastBlockId    = Base58.tryDecodeWithLimit((lastBlock \ "signature").as[String]).get
    val lastBlockTS    = (lastBlock \ "timestamp").as[Long]
    val lastBlockCData = (lastBlock \ "nxt-consensus").as[NxtLikeConsensusBlockData]
    val lastBlockVRF   = (lastBlock \ "VRF").asOpt[String].map(str => ByteStr.decodeBase58(str).get)

    (lastBlockId, lastBlockTS, lastBlockCData, lastBlockVRF)
  }

  def blockTimestamp(h: Int): Long = {
    (Json.parse(
      nodes.head
        .get(s"/blocks/at/$h")
        .getResponseBody
    ) \ "timestamp").as[Long]
  }

  def blockSignature(h: Int): Array[Byte] = {
    Base58
      .tryDecodeWithLimit(
        (Json.parse(
          nodes.head
            .get(s"/blocks/at/$h")
            .getResponseBody
        ) \ "signature").as[String]
      )
      .get
  }

  private val vrfActivationHeight = 20

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(3))
      .overrideBase(
        _.raw(
          s"""
             |waves {
             |  blockchain {
             |    custom {
             |      functionality {
             |        pre-activated-features = {
             |          2 = 0
             |          3 = 0
             |          4 = 0
             |          5 = 0
             |          6 = 0
             |          7 = 0
             |          8 = 0
             |          15 = $vrfActivationHeight
             |        }
             |      }
             |    }
             |  }
             |}
        """.stripMargin
        )
      )
      .overrideBase(_.nonMiner)
      .withDefault(3)
      .withSpecial(_.raw("waves.miner.enable = yes"))
      .buildNonConflicting()

  private def generatorSignature(signature: Array[Byte], publicKey: PublicKey): Array[Byte] = {
    val s = new Array[Byte](crypto.DigestLength * 2)
    System.arraycopy(signature, 0, s, 0, crypto.DigestLength)
    System.arraycopy(publicKey.arr, 0, s, crypto.DigestLength, crypto.DigestLength)
    crypto.fastHash(s)
  }

  def forgeBlock(
      height: Int,
      signerPK: KeyPair
  )(updateDelay: Long => Long = identity, updateBaseTarget: Long => Long = identity): Block = {

    val ggParentTS =
      if (height >= 3)
        Some(
          (Json
            .parse(nodes.head.get(s"/blocks/at/${height - 2}").getResponseBody) \ "timestamp").as[Long]
        )
      else None

    val (lastBlockId, lastBlockTS, lastBlockCData, lastBlockVRF) = blockInfo(height)
    val genSig: ByteStr =
      if (height + 1 < vrfActivationHeight)
        ByteStr(generatorSignature(lastBlockCData.generationSignature.arr, signerPK.publicKey))
      else
        crypto.signVRF(signerPK.privateKey, lastBlockVRF.getOrElse(lastBlockCData.generationSignature).arr)

    val hitSource =
      if (height + 1 < vrfActivationHeight)
        genSig
      else
        crypto.verifyVRF(genSig, lastBlockVRF.getOrElse(lastBlockCData.generationSignature).arr, signerPK.publicKey).explicitGet()

    val posCalculator = FairPoSCalculator.V1
    val version       = if (height + 1 < vrfActivationHeight) 3.toByte else 5.toByte

    val validBlockDelay: Long = updateDelay(
      posCalculator
        .calculateDelay(
          hit(hitSource.arr),
          lastBlockCData.baseTarget,
          nodes.head.accountBalances(signerPK.toAddress.toString)._2
        )
    )

    val baseTarget: Long = updateBaseTarget(
      posCalculator
        .calculateBaseTarget(
          10,
          height,
          lastBlockCData.baseTarget,
          lastBlockTS,
          ggParentTS,
          lastBlockTS + validBlockDelay
        )
    )

    Block
      .buildAndSign(
        version = version,
        timestamp = lastBlockTS + validBlockDelay,
        reference = ByteStr(lastBlockId),
        baseTarget = baseTarget,
        generationSignature = genSig,
        txs = Nil,
        signer = signerPK,
        featureVotes = Seq.empty,
        rewardVote = -1L,
        stateHash = None,
        challengedHeader = None
      )
      .explicitGet()
  }

  private def hit(generatorSignature: Array[Byte]): BigInt = BigInt(1, generatorSignature.take(8).reverse)
}
