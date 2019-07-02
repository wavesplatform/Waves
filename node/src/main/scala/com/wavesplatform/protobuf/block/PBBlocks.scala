package com.wavesplatform.protobuf.block
import cats.instances.all._
import cats.syntax.traverse._
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.block.SignerData
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.{PBTransactions, VanillaTransaction}
import com.wavesplatform.protobuf.utils.PBUtils

object PBBlocks {
  def vanilla(block: PBCachedBlock, unsafe: Boolean = false): Either[ValidationError, VanillaBlock] = {
    def create(version: Int,
               timestamp: Long,
               reference: ByteStr,
               consensusData: NxtLikeConsensusBlockData,
               transactionData: Seq[VanillaTransaction],
               featureVotes: Set[Short],
               generator: PublicKey,
               signature: ByteStr): VanillaBlock = {
      new VanillaBlock(timestamp, version.toByte, reference, SignerData(generator, signature), consensusData, transactionData, featureVotes)
    }

    if (block.block.getHeader.version > 3)
      Right(PBBlockAdapter(block))
    else for {
      header       <- Right(block.header)
      transactions <- block.transactions.map(PBTransactions.vanilla(_, unsafe)).toVector.sequence
      result = create(
        header.version,
        header.timestamp,
        ByteStr(header.reference.toByteArray),
        NxtLikeConsensusBlockData(header.baseTarget, ByteStr(header.generationSignature.toByteArray)),
        transactions,
        header.featureVotes.map(intToShort).toSet,
        PublicKey(header.generator.toByteArray),
        ByteStr(block.signature)
      )
    } yield result
  }

  def protobuf(block: VanillaBlock): PBCachedBlock = {
    block match {
      case a: PBBlockAdapter =>
        a.block

      case _ =>
        import block._
        import signerData._

        new PBBlock(
          Some(protobufHeaderAndSignature(block)._1),
          PBUtils.toByteStringUnsafe(signature),
          transactionData.map(PBTransactions.protobuf(_).transaction)
        )
    }
  }

  def protobufHeaderAndSignature(h: VanillaBlockHeader): (PBBlock.Header, Array[Byte]) = h match {
    case a: PBBlockAdapter =>
      (a.block.header, a.block.signature)

    case _ =>
      import h._
      import consensusData._
      import signerData._
      val header = PBBlock.Header(
        AddressScheme.current.chainId,
        PBUtils.toByteStringUnsafe(reference),
        baseTarget,
        PBUtils.toByteStringUnsafe(generationSignature),
        featureVotes.map(shortToInt).toSeq,
        timestamp,
        version,
        PBUtils.toByteStringUnsafe(generator)
      )
      (header, h.signerData.signature)
  }

  def vanillaHeader(h: PBBlock.Header, signature: Array[Byte]): VanillaBlockHeader = {
    val block = PBBlock()
      .withHeader(h)
      .withSignature(PBUtils.toByteStringUnsafe(signature))
    PBBlockAdapter(block) // Not contains transactionsCount
  }

  def clearChainId(block: PBBlock): PBBlock = {
    block.update(
      _.header.chainId := 0,
      _.transactions.foreach(_.transaction.chainId := 0)
    )
  }

  def addChainId(block: PBBlock): PBBlock = {
    val chainId = AddressScheme.current.chainId

    block.update(
      _.header.chainId := chainId,
      _.transactions.foreach(_.transaction.chainId := chainId)
    )
  }

  private[this] def shortToInt(s: Short): Int = {
    java.lang.Short.toUnsignedInt(s)
  }

  private[this] def intToShort(int: Int): Short = {
    require(int >= 0 && int <= 65535, s"Short overflow: $int")
    int.toShort
  }
}
