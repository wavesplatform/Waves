package com.wavesplatform.protobuf.block
import cats.instances.all._
import cats.syntax.traverse._
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.SignerData
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.{PBTransactions, VanillaTransaction}
import com.wavesplatform.transaction.TxValidationError.GenericError
import scorex.crypto.hash.Digest32

object PBBlocks {
  def vanilla(block: PBBlock, unsafe: Boolean = false): Either[ValidationError, VanillaBlock] = {
    def create(version: Int,
               timestamp: Long,
               reference: ByteStr,
               consensusData: NxtLikeConsensusBlockData,
               transactionData: Seq[VanillaTransaction],
               transactionTreeHash: Digest32,
               minerBalancesTreeHash: Digest32,
               minerEffectiveBalancesTreeHash: Digest32,
               featureVotes: Set[Short],
               generator: PublicKey,
               signature: ByteStr): VanillaBlock = {
      VanillaBlock(
        timestamp,
        version.toByte,
        reference,
        SignerData(generator, signature),
        consensusData,
        transactionTreeHash,
        minerBalancesTreeHash,
        minerEffectiveBalancesTreeHash,
        transactionData,
        featureVotes
      )
    }

    for {
      header       <- block.header.toRight(GenericError("No block header"))
      transactions <- block.transactions.map(PBTransactions.vanilla(_, unsafe)).toVector.sequence
      result = create(
        header.version,
        header.timestamp,
        ByteStr(header.reference.toByteArray),
        NxtLikeConsensusBlockData(header.baseTarget, ByteStr(header.generationSignature.toByteArray)),
        transactions,
        Digest32 @@ header.transactionTreeHash.toByteArray,
        Digest32 @@ header.minerBalancesTreeHash.toByteArray,
        Digest32 @@ header.minerEffectiveBalancesTreeHash.toByteArray,
        header.featureVotes.map(intToShort).toSet,
        PublicKey(header.generator.toByteArray),
        ByteStr(block.signature.toByteArray)
      )
    } yield result
  }

  def protobuf(block: VanillaBlock): PBBlock = {
    import block._
    import consensusData._
    import signerData._

    new PBBlock(
      Some(
        PBBlock.Header(
          0: Byte,
          ByteString.copyFrom(reference),
          baseTarget,
          ByteString.copyFrom(generationSignature),
          featureVotes.map(shortToInt).toSeq,
          timestamp,
          version,
          ByteString.copyFrom(generator),
          ByteString.copyFrom(transactionTreeHash),
          ByteString.copyFrom(minerWavesBalancesTreeHash),
          ByteString.copyFrom(minerEffectiveBalancesTreeHash)
        )
      ),
      ByteString.copyFrom(signature),
      transactionData.map(PBTransactions.protobuf)
    )
  }

  def clearChainId(block: PBBlock): PBBlock = {
    block.update(
      _.header.chainId := 0,
      _.transactions.foreach(_.transaction.chainId := 0)
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
