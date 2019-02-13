package com.wavesplatform.block.protobuf

import com.wavesplatform.serialization.protobuf.utils.PBUtils
import com.wavesplatform.transaction.protobuf.Transaction._
import com.wavesplatform.{block => vb}
import monix.eval.Coeval
import play.api.libs.json.{JsNumber, JsObject, Json}

trait PBBlockImplicits {
  implicit class PBBlockVanillaAdapter(block: Block)
      extends VanillaBlock(
        block.timestamp,
        block.version.toByte,
        block.reference,
        vb.SignerData(block.getSignerData.generator, block.getSignerData.signature),
        com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.getConsensusData.baseTarget, block.getConsensusData.generationSignature),
        block.transactions.map(_.toVanillaAdapter),
        block.featureVotes.map(intToShort)
      ) {

    def underlying = block

    override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBUtils.encodeDeterministic(block))
    override val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(block.version match {
      case 1 | 2 | 3 => block.toVanilla.bytesWithoutSignature()
      case _     => PBUtils.encodeDeterministic(block.copy(signerData = None))
    })


    override val headerJson: Coeval[JsObject] = Coeval.evalOnce(block.version match {
      case 1 | 2 | 3 =>
        block.toVanilla.headerJson()

      case _ =>
        val baseJson = Json.toJson(block.withTransactions(Nil)).as[JsObject]
        baseJson + ("transactionCount" -> JsNumber(block.transactions.length))

    })
    override val json: Coeval[JsObject] = Coeval.evalOnce(block.version match {
      case 1 | 2 | 3 => block.toVanilla.json()
      case _ => Json.toJson(block).as[JsObject]
    })

    override def toString: String = s"PBBlock(${signerData.signature} -> ${reference.trim}, txs=${transactionData.size}, features=$featureVotes)"
    override def hashCode(): Int           = block.hashCode()
    override def equals(obj: Any): Boolean = obj match {
      case a: PBBlockVanillaAdapter => block.equals(a.underlying)
      case _ => block.equals(obj)
    }
  }

  implicit class PBBlockImplicitConversionOps(block: Block) {
    def toVanillaAdapter = PBBlockVanillaAdapter(block)

    def toVanilla: vb.Block = {
      vb.Block.createLegacy(
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
