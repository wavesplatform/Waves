package com.wavesplatform.block.protobuf

import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.serialization.protobuf.PBSerializable._
import com.wavesplatform.serialization.protobuf.{PBMappers, PBSerializable, PBSerializableUnsigned}
import com.wavesplatform.transaction.protobuf.{ChainId, PBSignedTransactionImplicits}
import com.wavesplatform.{block => vb}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.annotation.switch

trait PBBlockImplicits { self: PBSignedTransactionImplicits with PBMappers =>
  implicit def blockToHeader(block: PBBlock): PBBlock.Header                       = block.getHeader.getHeader
  implicit def blockSignedHeaderToHeader(sh: PBBlock.SignedHeader): PBBlock.Header = sh.getHeader

  implicit val PBBlockPBSerializableInstance = new PBSerializable[PBBlock] with PBSerializableUnsigned[PBBlock] {
    override def protoBytes(value: PBBlock): SerializedT         = PBBlockSerialization.signedBytes(value)
    override def protoBytesUnsigned(value: PBBlock): SerializedT = PBBlockSerialization.unsignedBytes(value)
  }

  implicit class PBBlockSignedHeaderConversionOps(signed: PBBlock.SignedHeader) {
    def toVanilla: vb.BlockHeader = {
      new vb.BlockHeader(
        signed.getHeader.timestamp,
        signed.getHeader.version.toByte,
        signed.getHeader.reference.toByteArray,
        vb.SignerData(PublicKeyAccount(signed.getHeader.generator.toByteArray), signed.signature.toByteArray),
        NxtLikeConsensusBlockData(signed.getHeader.baseTarget, signed.getHeader.generationSignature.toByteArray),
        0,
        signed.getHeader.featureVotes.map(intToShort).toSet
      )
    }
  }

  implicit class PBBlockHeaderConversionOps(header: PBBlock.Header) {
    def toVanilla: vb.BlockHeader = {
      PBBlock.SignedHeader(Some(header)).toVanilla
    }
  }

  implicit class VanillaHeaderConversionOps(header: vb.BlockHeader) {
    def toPBHeader: PBBlock.SignedHeader = {
      val h = PBBlock.Header(
        header.reference,
        header.consensusData.baseTarget,
        header.consensusData.generationSignature,
        header.featureVotes.map(shortToInt).toSeq,
        header.timestamp,
        header.version,
        header.signerData.generator.publicKey: ByteStr
      )

      PBBlock.SignedHeader(Some(h), header.signerData.signature)
    }
  }

  class PBBlockVanillaAdapter(block: PBBlock)
      extends VanillaBlock(
        block.getHeader.timestamp,
        block.getHeader.version.toByte,
        block.getHeader.reference.byteStr,
        vb.SignerData(block.getHeader.generator.publicKeyAccount, block.getHeader.signature.byteStr),
        com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.getHeader.baseTarget, block.getHeader.generationSignature.byteStr),
        block.transactions.map(_.toVanillaOrAdapter),
        block.getHeader.featureVotes.map(intToShort).toSet
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
        block.getHeader.timestamp,
        block.getHeader.version.toByte,
        block.getHeader.reference.toByteArray,
        vb.SignerData(PublicKeyAccount(block.getHeader.generator.toByteArray), block.getHeader.signature.toByteArray),
        com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.getHeader.baseTarget, block.getHeader.generationSignature.toByteArray),
        block.transactions.map(_.toVanilla),
        block.getHeader.featureVotes.map(intToShort).toSet
      )
    }
  }

  implicit class VanillaBlockConversions(block: VanillaBlock) {
    def toPB: PBBlock = block match {
      case a: PBBlockVanillaAdapter =>
        a.underlying

      case _ =>
        import block._
        import consensusData._
        import signerData._

        new PBBlock(
          ChainId.empty,
          Some(
            PBBlock.SignedHeader(
              Some(
                PBBlock.Header(reference,
                               baseTarget,
                               generationSignature,
                               featureVotes.map(shortToInt).toSeq,
                               timestamp,
                               version,
                               generator.publicKey: ByteStr)),
              signature
            )),
          transactionData.map(_.toPB)
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
