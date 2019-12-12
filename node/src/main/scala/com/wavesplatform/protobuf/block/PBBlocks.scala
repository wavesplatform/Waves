package com.wavesplatform.protobuf.block
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.PBTransactions

import scala.util.Try

object PBBlocks {
  def vanilla(block: PBBlock, unsafe: Boolean = false): Try[VanillaBlock] = Try {
    require(block.header.isDefined, "block header is missing")
    val header       = block.getHeader
    val transactions = block.transactions.map(PBTransactions.vanilla(_, unsafe).explicitGet())

    VanillaBlock(
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
      ),
      ByteStr(block.signature.toByteArray),
      transactions
    )
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
          header.featureVotes.map(shortToInt),
          header.timestamp,
          header.version,
          ByteString.copyFrom(generator),
          header.rewardVote,
          ByteString.copyFrom(header.transactionsRoot)
        )
      ),
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
