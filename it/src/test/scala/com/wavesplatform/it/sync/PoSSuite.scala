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
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData

import scala.concurrent.Await
import scala.concurrent.duration._

class PoSSuite extends BaseTransactionSuite {

  implicit val nxtCDataReads = Reads { json =>
    val bt = (json \ "base-target").as[Long]
    val gs = (json \ "generation-signature").as[String]

    JsSuccess(NxtLikeConsensusBlockData(bt, ByteStr.decodeBase58(gs).get))
  }

  test("Block with invalid timestamp (min valid time - 1 second)") {
    nodes.waitFor[Int]("height")(5 seconds)(_.height, _.head < 10)

    val height = nodes.last.height

    nodes.last.close()

    val ggParentTS = (Json.parse(nodes.head.get(s"/blocks/at/${height - 2}").getResponseBody) \ "timestamp").as[Long]

    val lastBlock = Json.parse(nodes.head.get("/blocks/last").getResponseBody)

    val lastBlockId = Base58.decode((lastBlock \ "signature").as[String]).get

    val lastBlockTS = (lastBlock \ "timestamp").as[Long]

    val lastBlockCData = (lastBlock \ "nxt-consensus").as[NxtLikeConsensusBlockData]

    val signerPK = PrivateKeyAccount(Base58.decode(nodeConfigs.last.getString("private-key")).get)

    val genSig: ByteStr = ByteStr(generatorSignature(lastBlockCData.generationSignature.arr, signerPK.publicKey))

    val validBlockDelay: Long = NxtPoSCalculator
      .calculateDelay(
        hit(genSig.arr),
        lastBlockCData.baseTarget,
        nodes.head.accountBalances(signerPK.address)._2
      )

    val bastTarget: Long = NxtPoSCalculator
      .calculateBaseTarget(
        60,
        height,
        lastBlockCData.baseTarget,
        lastBlockTS,
        Some(ggParentTS),
        validBlockDelay
      )

    val cData: NxtLikeConsensusBlockData = NxtLikeConsensusBlockData(bastTarget, genSig)

    val block = Block
      .buildAndSign(
        version = 3: Byte,
        timestamp = lastBlockTS + validBlockDelay - 1000,
        reference = ByteStr(lastBlockId),
        consensusData = cData,
        transactionData = Nil,
        signer = signerPK,
        featureVotes = Set.empty
      )
      .explicitGet()

    val futureResponse = nodes.head.sendByNetwork(RawBytes.from(block))

    Await.result(futureResponse, 5 seconds)
  }

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(3))
      .overrideBase(_.raw("waves.miner.enabled = false"))
      .withDefault(3)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  private def generatorSignature(signature: Array[Byte], publicKey: Array[Byte]): Array[Byte] = {
    val s = new Array[Byte](crypto.DigestSize * 2)
    System.arraycopy(signature, 0, s, 0, crypto.DigestSize)
    System.arraycopy(publicKey, 0, s, crypto.DigestSize, crypto.DigestSize)
    crypto.fastHash(s)
  }

  private def hit(generatorSignature: Array[Byte]): BigInt = BigInt(1, generatorSignature.take(8).reverse)
}
