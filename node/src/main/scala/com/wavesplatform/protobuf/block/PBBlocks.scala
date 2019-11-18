package com.wavesplatform.protobuf.block
import cats.instances.all._
import cats.syntax.traverse._
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.{PBTransactions, VanillaTransaction}
import com.wavesplatform.transaction.TxValidationError.GenericError

object PBBlocks {
  def vanilla(block: PBBlock, unsafe: Boolean = false): Either[ValidationError, VanillaBlock] = {
    def create(version: Int,
               timestamp: Long,
               reference: ByteStr,
               baseTarget: Long,
               generationSignature: ByteStr,
               transactionData: Seq[VanillaTransaction],
               featureVotes: Set[Short],
               rewardVote: Long,
               generator: PublicKey,
               signature: ByteStr,
               merkle: ByteStr): VanillaBlock = {
      VanillaBlock(
        BlockHeader(
          version.toByte, timestamp, reference, baseTarget, generationSignature, generator, featureVotes, rewardVote, merkle
        ),
        signature,
        transactionData
      )
    }

    for {
      header       <- block.header.toRight(GenericError("No block header"))
      transactions <- block.transactions.map(PBTransactions.vanilla(_, unsafe)).toVector.sequence
      result = create(
        header.version,
        header.timestamp,
        ByteStr(header.reference.toByteArray),
        header.baseTarget,
        ByteStr(header.generationSignature.toByteArray),
        transactions,
        header.featureVotes.map(intToShort).toSet,
        header.rewardVote,
        PublicKey(header.generator.toByteArray),
        ByteStr(block.signature.toByteArray),
        ByteStr(header.merkle.toByteArray)
      )
    } yield result
  }

  def protobuf(block: VanillaBlock): PBBlock = {
    import block._
    import block.header._

    new PBBlock(
      Some(
        PBBlock.Header(
          AddressScheme.current.chainId,
          ByteString.copyFrom(reference),
          baseTarget,
          ByteString.copyFrom(generationSignature),
          header.featureVotes.map(shortToInt).toSeq,
          header.timestamp,
          header.version,
          ByteString.copyFrom(generator),
          header.rewardVote,
          ByteString.copyFrom(header.merkle)
        )),
      ByteString.copyFrom(block.signature),
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
