package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.consensus.FairPoSCalculator
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto
import com.wavesplatform.http.DebugMessage
import com.wavesplatform.it.api.AsyncNetworkApi.NodeAsyncNetworkApi
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{Node, NodeConfigs, WaitForHeight2}
import com.wavesplatform.network.RawBytes
import com.wavesplatform.state._
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import com.wavesplatform.utils.Base58
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}
import play.api.libs.json.{JsSuccess, Json, Reads}

class ForkSuite extends FunSuite with Matchers with NodesFromDocker with WaitForHeight2 with CancelAfterFailure {
  private def nodeA: Node = nodes.last
  private def nodeB: Node = nodes.head

  val signerNodeA = PrivateKeyAccount.fromSeed(nodeConfigs.last.getString("account-seed")).explicitGet()

  implicit val nxtCDataReads = Reads { json =>
    val bt = (json \ "base-target").as[Long]
    val gs = (json \ "generation-signature").as[String]

    JsSuccess(NxtLikeConsensusBlockData(bt, ByteStr.decodeBase58(gs).get))
  }

  test("Accept correct block") {
    val height = nodeA.height
    val block  = forgeBlock(nodeB, height, signerNodeA)()

    waitForBlockTime(block)

    nodeB.printDebugMessage(DebugMessage(s"Send block for $height"))
    nodeB.sendByNetwork(RawBytes.from(block))

    nodeA.waitForHeight(height + 1)
    nodeB.waitForHeight(height + 1)

    docker.disconnectFromNetwork(dockerNodes().head) // disconnect nodeB

    val tx = TransferTransactionV1.selfSigned(None, signerNodeA, signerNodeA.toAddress, 1, 1, None, 1, Array.emptyByteArray).explicitGet()
    nodeA.printDebugMessage(DebugMessage(s"Send block for $height"))
    nodeA.sendByNetwork(RawBytes.from(tx))

    nodeA.waitForHeight(nodeA.height + 1)

    docker.connectToNetwork(Seq(dockerNodes().head))

    val maxHeight = nodes.map(_.height).max
    nodeB.waitForHeight(maxHeight)

    blockSignature(nodeA, maxHeight) should not be blockSignature(nodeB, maxHeight)
  }

  def waitForBlockTime(block: Block): Unit = {
    val timeout = block.timestamp - System.currentTimeMillis()

    if (timeout > 0) Thread.sleep(timeout)
  }

  def blockInfo(node: Node, height: Int): (Array[Byte], Long, NxtLikeConsensusBlockData) = {
    val lastBlock      = Json.parse(node.get(s"/blocks/at/$height").getResponseBody)
    val lastBlockId    = Base58.decode((lastBlock \ "signature").as[String]).get
    val lastBlockTS    = (lastBlock \ "timestamp").as[Long]
    val lastBlockCData = (lastBlock \ "nxt-consensus").as[NxtLikeConsensusBlockData]

    (lastBlockId, lastBlockTS, lastBlockCData)
  }

  def blockTimestamp(node: Node, h: Int): Long = {
    (Json.parse(
      node
        .get(s"/blocks/at/$h")
        .getResponseBody
    ) \ "timestamp").as[Long]
  }

  def blockSignature(node: Node, h: Int): Array[Byte] = {
    Base58
      .decode(
        (Json.parse(
          node
            .get(s"/blocks/at/$h")
            .getResponseBody
        ) \ "signature").as[String])
      .get
  }

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(1))
      .overrideBase(
        _.raw(
          """
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
          |        }
          |      }
          |    }
          |  }
          |}
        """.stripMargin
        ))
      .withDefault(2)
      .buildNonConflicting()

  private def generatorSignature(signature: Array[Byte], publicKey: Array[Byte]): Array[Byte] = {
    val s = new Array[Byte](crypto.DigestSize * 2)
    System.arraycopy(signature, 0, s, 0, crypto.DigestSize)
    System.arraycopy(publicKey, 0, s, crypto.DigestSize, crypto.DigestSize)
    crypto.fastHash(s)
  }

  def forgeBlock(node: Node, height: Int, signerPK: PrivateKeyAccount)(updateDelay: Long => Long = identity,
                                                                       updateBaseTarget: Long => Long = identity,
                                                                       updateGenSig: ByteStr => ByteStr = identity): Block = {

    val ggParentTS =
      if (height >= 3)
        Some(
          (Json
            .parse(node.get(s"/blocks/at/${height - 2}").getResponseBody) \ "timestamp").as[Long])
      else None

    val (lastBlockId, lastBlockTS, lastBlockCData) = blockInfo(node, height)

    val genSig: ByteStr =
      updateGenSig(
        ByteStr(generatorSignature(lastBlockCData.generationSignature.arr, signerPK.publicKey))
      )

    val validBlockDelay: Long = updateDelay(
      FairPoSCalculator
        .calculateDelay(
          hit(genSig.arr),
          lastBlockCData.baseTarget,
          node.accountBalances(signerPK.address)._2
        )
    )

    val bastTarget: Long = updateBaseTarget(
      FairPoSCalculator
        .calculateBaseTarget(
          10,
          height,
          lastBlockCData.baseTarget,
          lastBlockTS,
          ggParentTS,
          lastBlockTS + validBlockDelay
        )
    )

    val cData: NxtLikeConsensusBlockData = NxtLikeConsensusBlockData(bastTarget, genSig)

    Block
      .buildAndSign(
        version = 3: Byte,
        timestamp = lastBlockTS + validBlockDelay,
        reference = ByteStr(lastBlockId),
        consensusData = cData,
        transactionData = Nil,
        signer = signerPK,
        featureVotes = Set.empty
      )
      .explicitGet()
  }

  def forgeMicroblock(node: Node, height: Int, signerPK: PrivateKeyAccount)(updateDelay: Long => Long = identity,
                                                                            updateBaseTarget: Long => Long = identity,
                                                                            updateGenSig: ByteStr => ByteStr = identity): Block = {

    val ggParentTS =
      if (height >= 3)
        Some(
          (Json
            .parse(node.get(s"/blocks/at/${height - 2}").getResponseBody) \ "timestamp").as[Long])
      else None

    val (lastBlockId, lastBlockTS, lastBlockCData) = blockInfo(node, height)

    val genSig: ByteStr =
      updateGenSig(
        ByteStr(generatorSignature(lastBlockCData.generationSignature.arr, signerPK.publicKey))
      )

    val validBlockDelay: Long = updateDelay(
      FairPoSCalculator
        .calculateDelay(
          hit(genSig.arr),
          lastBlockCData.baseTarget,
          node.accountBalances(signerPK.address)._2
        )
    )

    val bastTarget: Long = updateBaseTarget(
      FairPoSCalculator
        .calculateBaseTarget(
          10,
          height,
          lastBlockCData.baseTarget,
          lastBlockTS,
          ggParentTS,
          lastBlockTS + validBlockDelay
        )
    )

    val cData: NxtLikeConsensusBlockData = NxtLikeConsensusBlockData(bastTarget, genSig)

    Block
      .buildAndSign(
        version = 3: Byte,
        timestamp = lastBlockTS + validBlockDelay,
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
