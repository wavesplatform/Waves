package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.consensus.NxtPoSCalculator
import com.wavesplatform.crypto
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.AsyncNetworkApi.NodeAsyncNetworkApi
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.network.RawBytes
import com.wavesplatform.state._
import com.wavesplatform.utils.Base58
import play.api.libs.json.{JsSuccess, Json, Reads}
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, SignerData}
import scorex.consensus.nxt.NxtLikeConsensusBlockData

import scala.concurrent.duration._
import scala.util.Random

class PoSSuite extends BaseTransactionSuite {

  val signerPK = PrivateKeyAccount.fromSeed(nodeConfigs.last.getString("account-seed")).explicitGet()

  implicit val nxtCDataReads = Reads { json =>
    val bt = (json \ "base-target").as[Long]
    val gs = (json \ "generation-signature").as[String]

    JsSuccess(NxtLikeConsensusBlockData(bt, ByteStr.decodeBase58(gs).get))
  }

  test("Accept correct block") {

    val height = nodes.last.height
    val block  = forgeBlock(height, signerPK)()

    waitForBlockTime(block)

    nodes.head.sendByNetwork(RawBytes.from(block))

    waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig sameElements block.uniqueId.arr
  }

  test("Reject block with invalid delay") {
    val height = nodes.last.height
    val block  = forgeBlock(height, signerPK)(updateDelay = _ - 1000)

    waitForBlockTime(block)

    nodes.head.sendByNetwork(RawBytes.from(block))

    waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.uniqueId.arr
  }

  test("Reject block with invalid BT") {
    val height = nodes.last.height
    val block  = forgeBlock(height, signerPK)(updateBaseTarget = _ + 2)

    waitForBlockTime(block)

    nodes.head.sendByNetwork(RawBytes.from(block))

    waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.uniqueId.arr
  }

  test("Reject block with invalid generation signature") {
    val height = nodes.last.height
    val block = forgeBlock(height, signerPK)(updateGenSig = (gs: ByteStr) => {
      val arr  = gs.arr
      val init = arr.init
      Random.nextBytes(arr)
      ByteStr(init :+ arr.last)
    })

    waitForBlockTime(block)

    nodes.head.sendByNetwork(RawBytes.from(block))

    waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be block.uniqueId.arr
  }

  test("Reject block with invalid signature") {
    val otherNodePK = PrivateKeyAccount.fromSeed(nodeConfigs.head.getString("account-seed")).explicitGet()

    val height = nodes.last.height
    val block  = forgeBlock(height, signerPK)(updateBaseTarget = _ + 2)

    val resignedBlock =
      block
        .copy(signerData = SignerData(signerPK, ByteStr(crypto.sign(otherNodePK, block.bytes()))))

    waitForBlockTime(resignedBlock)

    nodes.head.sendByNetwork(RawBytes.from(resignedBlock))

    waitForHeight(height + 1)

    val newBlockSig = blockSignature(height + 1)

    newBlockSig should not be resignedBlock.uniqueId.arr
  }

  def waitForBlockTime(block: Block): Unit = {
    val timeout = block.timestamp - System.currentTimeMillis()

    if (timeout > 0) Thread.sleep(timeout)
  }

  def previousBlockInfo(height: Int): (Array[Byte], Long, NxtLikeConsensusBlockData) = {
    val lastBlock      = Json.parse(nodes.head.get(s"/blocks/at/$height").getResponseBody)
    val lastBlockId    = Base58.decode((lastBlock \ "signature").as[String]).get
    val lastBlockTS    = (lastBlock \ "timestamp").as[Long]
    val lastBlockCData = (lastBlock \ "nxt-consensus").as[NxtLikeConsensusBlockData]

    (lastBlockId, lastBlockTS, lastBlockCData)
  }

  def blockSignature(h: Int): Array[Byte] = {
    Base58
      .decode(
        (Json.parse(
          nodes.last
            .get(s"/blocks/at/$h")
            .getResponseBody
        ) \ "signature").as[String])
      .get
  }

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(3))
      .withDefault(3)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  private def generatorSignature(signature: Array[Byte], publicKey: Array[Byte]): Array[Byte] = {
    val s = new Array[Byte](crypto.DigestSize * 2)
    System.arraycopy(signature, 0, s, 0, crypto.DigestSize)
    System.arraycopy(publicKey, 0, s, crypto.DigestSize, crypto.DigestSize)
    crypto.fastHash(s)
  }

  def waitForHeight(h: Int): Unit = {
    nodes.waitFor[Int]("height")(1.seconds)(_.height, _.head > h)
  }

  def forgeBlock(height: Int, signerPK: PrivateKeyAccount)(updateDelay: Long => Long = identity,
                                                           updateBaseTarget: Long => Long = identity,
                                                           updateGenSig: ByteStr => ByteStr = identity): Block = {

    val ggParentTS =
      if (height >= 3)
        Some(
          (Json
            .parse(nodes.head.get(s"/blocks/at/${height - 2}").getResponseBody) \ "timestamp").as[Long])
      else None

    val (lastBlockId, lastBlockTS, lastBlockCData) = previousBlockInfo(height)

    val genSig: ByteStr =
      updateGenSig(
        ByteStr(generatorSignature(lastBlockCData.generationSignature.arr, signerPK.publicKey))
      )

    val validBlockDelay: Long = updateDelay(
      NxtPoSCalculator
        .calculateDelay(
          hit(genSig.arr),
          lastBlockCData.baseTarget,
          nodes.head.accountBalances(signerPK.address)._2
        )
    )

    val bastTarget: Long = updateBaseTarget(
      NxtPoSCalculator
        .calculateBaseTarget(
          60,
          height,
          lastBlockCData.baseTarget,
          lastBlockTS,
          ggParentTS,
          validBlockDelay
        )
    )

    val cData: NxtLikeConsensusBlockData = NxtLikeConsensusBlockData(bastTarget, genSig)

    Block
      .buildAndSign(
        version = 3: Byte,
        timestamp = lastBlockTS + validBlockDelay /*- 1000*/,
        reference = ByteStr(lastBlockId),
        consensusData = cData,
        transactionData = Nil,
        signer = signerPK,
        featureVotes = Set.empty
      )
      .explicitGet()
  }

  private def hit(generatorSignature: Array[Byte]): BigInt = BigInt(1, generatorSignature.take(8).reverse)
}
