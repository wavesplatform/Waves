package com.wavesplatform.block.protobuf

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.serialization.protobuf.PBSerializable._
import com.wavesplatform.serialization.protobuf.{PBSerializable, PBSerializableUnsigned}
import com.wavesplatform.transaction.protobuf.ChainId
import com.wavesplatform.transaction.protobuf.PBSignedTransaction._
import com.wavesplatform.{block => vb}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.annotation.switch

trait PBBlockImplicits {
  implicit def blockToHeader(block: PBBlock): PBBlock.Header                       = block.header.header
  implicit def blockSignedHeaderToHeader(sh: PBBlock.SignedHeader): PBBlock.Header = sh.header

  implicit val PBBlockPBSerializableInstance = new PBSerializable[PBBlock] with PBSerializableUnsigned[PBBlock] {
    override def protoBytes(value: PBBlock): SerializedT = value.getOrComputeProtoBytes
    override def protoBytesUnsigned(value: PBBlock): SerializedT = value.getOrComputeProtoBytesUnsigned
  }

  implicit class PBBlockSignedHeaderConversionOps(signed: PBBlock.SignedHeader) {
    def toVanilla: vb.BlockHeader = {
      new vb.BlockHeader(
        signed.header.timestamp,
        signed.header.version.toByte,
        signed.header.reference,
        vb.SignerData(signed.header.generator, signed.signature),
        NxtLikeConsensusBlockData(signed.header.baseTarget, signed.header.generationSignature),
        0,
        signed.header.featureVotes.map(intToShort)
      )
    }
  }

  implicit class PBBlockHeaderConversionOps(header: PBBlock.Header) {
    def toVanilla: vb.BlockHeader = {
      PBBlock.SignedHeader(header).toVanilla
    }
  }

  implicit class VanillaHeaderConversionOps(header: vb.BlockHeader) {
    def toPBHeader: PBBlock.SignedHeader = {
      val h = PBBlock.Header(
        header.reference,
        header.consensusData.baseTarget,
        header.consensusData.generationSignature,
        header.featureVotes.map(shortToInt),
        header.timestamp,
        header.version,
        header.signerData.generator
      )

      PBBlock.SignedHeader(h, header.signerData.signature)
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

    override val bytes: Coeval[Array[Byte]] = Coeval(block.protoBytes)

    override val bytesWithoutSignature: Coeval[Array[Byte]] = (block.version: @switch) match {
      case 1 | 2 | 3 => Coeval.evalOnce(block.toVanilla.bytesWithoutSignature())
      case _         => Coeval(block.protoBytesUnsigned)
    }

    override val headerJson: Coeval[JsObject] = Coeval.evalOnce((block.version: @switch) match {
      case 1 | 2 | 3 =>
        block.toVanilla.headerJson()

      case _ =>
        ???
    })

    override val json: Coeval[JsObject] = Coeval.evalOnce((block.version: @switch) match {
      case 1 | 2 | 3 => block.toVanilla.json()
      case _         => ???
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
      new vb.Block(
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
          ChainId.empty,
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
