package com.wavesplatform.block.protobuf
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.block.SignerData
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.protobuf.{PBTransactions, VanillaTransaction}

object PBBlocks {
  def vanilla(block: PBBlock): Either[ValidationError, VanillaBlock] = {
    def create(version: Int,
               timestamp: Long,
               reference: ByteStr,
               consensusData: NxtLikeConsensusBlockData,
               transactionData: Seq[VanillaTransaction],
               featureVotes: Set[Short],
               generator: PublicKeyAccount,
               signature: ByteStr): VanillaBlock = {
      VanillaBlock(timestamp, version.toByte, reference, SignerData(generator, signature), consensusData, transactionData, featureVotes)
    }

    for {
      signedHeader <- block.header.toRight(GenericError("No block header"))
      header       <- signedHeader.header.toRight(GenericError("No block header"))
      transactions <- {
        val eithers = block.transactions.map(PBTransactions.vanilla(_))
        (eithers.find(_.isLeft): @unchecked) match {
          case None              => Right(eithers.map(_.right.get))
          case Some(Left(error)) => Left(error)
        }
      }
      result = create(
        header.version,
        header.timestamp,
        ByteStr(header.reference.toByteArray),
        NxtLikeConsensusBlockData(header.baseTarget, ByteStr(header.generationSignature.toByteArray)),
        transactions,
        header.featureVotes.map(intToShort).toSet,
        PublicKeyAccount(header.generator.toByteArray),
        ByteStr(signedHeader.signature.toByteArray)
      )
    } yield result
  }

  def protobuf(block: VanillaBlock): PBBlock = {
    import block._
    import consensusData._
    import signerData._

    new PBBlock(
      ByteString.EMPTY,
      Some(
        PBBlock.SignedHeader(
          Some(PBBlock.Header(
            ByteString.copyFrom(reference),
            baseTarget,
            ByteString.copyFrom(generationSignature),
            featureVotes.map(shortToInt).toSeq,
            timestamp,
            version,
            ByteString.copyFrom(generator.publicKey)
          )),
          ByteString.copyFrom(signature)
        )),
      transactionData.map(PBTransactions.protobuf)
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
