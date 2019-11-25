package com.wavesplatform.protobuf.block
import cats.instances.all._
import cats.syntax.traverse._
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.Block.{Header => PBHeader}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.TxValidationError.GenericError

object PBBlocks {
  def vanilla(header: PBBlock.Header): BlockHeader =
    BlockHeader(
      header.version.toByte,
      header.timestamp,
      ByteStr(header.reference.toByteArray),
      header.baseTarget,
      ByteStr(header.generationSignature.toByteArray),
      PublicKey(header.generator.toByteArray),
      header.featureVotes.map(intToShort),
      header.rewardVote,
      ByteStr(header.transactionsRoot.toByteArray)
    )

  def vanilla(block: PBBlock, unsafe: Boolean = false): Either[ValidationError, VanillaBlock] =
    for {
      header       <- block.header.toRight(GenericError("No block header"))
      transactions <- block.transactions.map(PBTransactions.vanilla(_, unsafe)).toVector.sequence
    } yield VanillaBlock(vanilla(header), ByteStr(block.signature.toByteArray), transactions)

  def protobuf(header: BlockHeader): PBHeader = PBBlock.Header(
    AddressScheme.current.chainId,
    ByteString.copyFrom(header.reference),
    header.baseTarget,
    ByteString.copyFrom(header.generationSignature),
    header.featureVotes.map(shortToInt),
    header.timestamp,
    header.version,
    ByteString.copyFrom(header.generator),
    header.rewardVote,
    ByteString.copyFrom(header.transactionsRoot)
  )

  def protobuf(block: VanillaBlock): PBBlock = {
    import block._

    new PBBlock(
      Some(protobuf(header)),
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
