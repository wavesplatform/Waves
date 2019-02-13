package com.wavesplatform.block.protobuf

import com.wavesplatform.serialization.protobuf.utils.PBUtils
import com.wavesplatform.transaction.protobuf.Transaction._
import com.wavesplatform.{block => vb}
import monix.eval.Coeval
import play.api.libs.json.{JsNumber, JsObject, Json}

trait PBBlockImplicits {
  implicit class PBBlockVanillaAdapter(block: Block) extends VanillaBlock(block.timestamp, block.version.toByte, block.reference, vb.SignerData(block.getSignerData.generator, block.getSignerData.signature), com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.getConsensusData.baseTarget, block.getConsensusData.generationSignature), block.transactions.map(_.toAdapter), block.featureVotes.map(intToShort)) {
    override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBUtils.encodeDeterministic(block))
    override val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(block.version match {
      case 1 | 2 => block.toVanilla.bytesWithoutSignature()
      case _     => PBUtils.encodeDeterministic(block.copy(signerData = None))
    })
    override val headerJson: Coeval[JsObject] = Coeval.evalOnce {
      val baseJson = Json.toJson(block.withTransactions(Nil)).as[JsObject]
      baseJson + ("transactionCount" -> JsNumber(block.transactions.length))
    }
    override val json: Coeval[JsObject]   = Coeval.evalOnce(Json.toJson(block).as[JsObject])
  }

  implicit class PBBlockImplicitConversionOps(block: Block) {
    def toAdapter = PBBlockVanillaAdapter(block)

    def toVanilla: vb.Block = {
      vb.Block(
        block.timestamp,
        block.version.toByte,
        block.reference,
        vb.SignerData(block.getSignerData.generator, block.getSignerData.signature),
        com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.getConsensusData.baseTarget, block.getConsensusData.generationSignature),
        block.transactions.map(_.toVanilla),
        block.featureVotes.map(intToShort)
      )
    }
  }

  implicit class VanillaBlockConversions(block: vb.Block) {
    def toPB: Block = {
      Block(
        block.reference,
        Some(Block.SignerData(block.signerData.generator, block.signerData.signature)),
        Some(Block.ConsensusData(block.consensusData.baseTarget, block.consensusData.generationSignature)),
        block.transactionData.map(_.toPB),
        block.featureVotes.map(shortToInt),
        block.timestamp,
        block.version
      )
    }
  }

  private[this] implicit def shortToInt(s: Short): Int = {
    java.lang.Short.toUnsignedInt(s)
  }

  private[this] def intToShort(int: Int): Short = {
    require(int >= 0 && int <= 65535, s"Short overflow: $int")
    int.toShort
  }
}

// object PBBlockImplicits extends PBBlockImplicits
