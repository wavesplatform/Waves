package com.wavesplatform.block.protobuf

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.transaction.protobuf.Transaction._
import com.wavesplatform.{block => vb}
import monix.eval.Coeval
import play.api.libs.json.{JsNumber, JsObject, Json}

import scala.annotation.switch

trait PBBlockImplicits {
  implicit def blockToBlockHeader(block: PBBlock): PBBlock.Header = block.header

  implicit class PBBlockHeaderConversionOps(header: PBBlock.Header) {
    def toVanilla: vb.BlockHeader = {
      new vb.BlockHeader(
        header.timestamp,
        header.version.toByte,
        header.reference,
        vb.SignerData(header.generator, header.signature),
        NxtLikeConsensusBlockData(header.baseTarget, header.generationSignature),
        0,
        header.featureVotes.map(intToShort)
      )
    }
  }

  implicit class VanillaHeaderConversionOps(header: vb.BlockHeader) {
    def toPBHeader: PBBlock.Header = {
      PBBlock.Header(
        header.reference,
        header.consensusData.baseTarget,
        header.consensusData.generationSignature,
        header.featureVotes.map(shortToInt),
        header.timestamp,
        header.version,
        header.signerData.generator,
        header.signerData.signature
      )
    }
  }

  class PBBlockVanillaAdapter(block: PBBlock)
      extends VanillaBlock(
        block.header.timestamp,
        block.header.version.toByte,
        block.header.reference,
        vb.SignerData(block.header.generator, block.header.signature),
        com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.header.baseTarget, block.header.generationSignature),
        block.transactions.map(_.toVanillaOrAdapter),
        block.header.featureVotes.map(intToShort)
      ) {

    def underlying: PBBlock = block

    def signature: ByteStr = this.signerData.signature

    override val bytes: Coeval[Array[Byte]] = block.protoBytes

    override val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce((block.version: @switch) match {
      case 1 | 2 | 3 => block.toVanilla.bytesWithoutSignature()
      case _         => block.protoBytesWithoutSignature()
    })

    override val headerJson: Coeval[JsObject] = Coeval.evalOnce((block.version: @switch) match {
      case 1 | 2 | 3 =>
        block.toVanilla.headerJson()

      case _ =>
        val baseJson = Json.toJson(block.withHeader(Block.Header.defaultInstance)).as[JsObject]
        baseJson + ("transactionCount" -> JsNumber(block.transactions.length))
    })

    override val json: Coeval[JsObject] = Coeval.evalOnce((block.version: @switch) match {
      case 1 | 2 | 3 => block.toVanilla.json()
      case _         => Json.toJson(block).as[JsObject]
    })

    /* protected override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce {
      block.transactions
        .map(_.toVanillaAdapter)
    } */

    override def toString: String = {
      s"PBBlock(${signerData.signature} -> ${reference.trim}, txs=${transactionData.size}, features=$featureVotes)"
    }

    override def hashCode(): Int = {
      block.hashCode()
    }

    override def equals(obj: Any): Boolean = obj match {
      case a: PBBlockVanillaAdapter => block.equals(a.underlying)
      case _                        => block.equals(obj)
    }
  }

  implicit class PBBlockImplicitConversionOps(block: PBBlock) {
    def isLegacy: Boolean = block.version <= 3

    def toVanillaAdapter = new PBBlockVanillaAdapter(block)

    def toVanilla: VanillaBlock = {
      vb.Block.createLegacy(
        block.header.timestamp,
        block.header.version.toByte,
        block.header.reference,
        vb.SignerData(block.header.generator, block.header.signature),
        com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.header.baseTarget, block.header.generationSignature),
        block.transactions.map(_.toVanilla),
        block.header.featureVotes.map(intToShort)
      )
    }
  }

  implicit class VanillaBlockConversions(block: VanillaBlock) {
    def toPB: PBBlock = block match {
      case a: PBBlockVanillaAdapter =>
        a.underlying

      case _ =>
        PBBlock.create(
          block.reference,
          block.consensusData.baseTarget,
          block.consensusData.generationSignature,
          block.featureVotes.map(shortToInt),
          block.timestamp,
          block.version,
          block.signerData.generator,
          block.signerData.signature,
          block.transactionData.map(_.toPB)
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
