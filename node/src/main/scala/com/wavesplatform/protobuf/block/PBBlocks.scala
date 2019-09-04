package com.wavesplatform.protobuf.block
import cats.instances.all._
import cats.syntax.traverse._
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.block.SignerData
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.{PBTransactions, VanillaTransaction}
import com.wavesplatform.transaction.TxValidationError.GenericError

object PBBlocks {
  def vanilla(block: PBBlock, unsafe: Boolean = false): Either[ValidationError, VanillaBlock] = {
    def create(version: Int,
               timestamp: Long,
               reference: ByteStr,
               consensusData: NxtLikeConsensusBlockData,
               transactionData: Seq[VanillaTransaction],
               featureVotes: Set[Short],
               rewardVote: Long,
               generator: PublicKey,
               signature: ByteStr): VanillaBlock = {
      VanillaBlock(timestamp, version.toByte, reference, SignerData(generator, signature), consensusData, transactionData, featureVotes, rewardVote)
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
        header.featureVotes.map(intToShort).toSet,
        header.rewardVote,
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
          AddressScheme.current.chainId,
          ByteString.copyFrom(reference),
          baseTarget,
          ByteString.copyFrom(generationSignature),
          featureVotes.map(shortToInt).toSeq,
          timestamp,
          version,
          ByteString.copyFrom(generator),
          rewardVote
        )),
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
