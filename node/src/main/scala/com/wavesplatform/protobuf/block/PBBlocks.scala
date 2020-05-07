package com.wavesplatform.protobuf.block

import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.block.Block.{Header => PBHeader}
import com.wavesplatform.protobuf.transaction.PBTransactions

import scala.util.Try

object PBBlocks {
  def vanilla(header: PBBlock.Header): BlockHeader =
    BlockHeader(
      header.version.toByte,
      header.timestamp,
      ByteStr(header.reference.toByteArray),
      header.baseTarget,
      ByteStr(header.generationSignature.toByteArray),
      PublicKey(header.generator.toByteArray),
      header.featureVotes.map(_.toShort),
      header.rewardVote,
      ByteStr(header.transactionsRoot.toByteArray)
    )

  def vanilla(block: PBBlock, unsafe: Boolean = false): Try[VanillaBlock] = Try {
    require(block.header.isDefined, "block header is missing")
    val header       = block.getHeader
    val transactions = block.transactions.map(PBTransactions.vanilla(_, unsafe).explicitGet())

    VanillaBlock(vanilla(header), ByteStr(block.signature.toByteArray), transactions)
  }

  def protobuf(header: BlockHeader): PBHeader = PBBlock.Header(
    AddressScheme.current.chainId,
    ByteString.copyFrom(header.reference.arr),
    header.baseTarget,
    ByteString.copyFrom(header.generationSignature.arr),
    header.featureVotes.map(_.toInt),
    header.timestamp,
    header.version,
    ByteString.copyFrom(header.generator.arr),
    header.rewardVote,
    ByteString.copyFrom(header.transactionsRoot.arr)
  )

  def protobuf(block: VanillaBlock): PBBlock = {
    import block._

    new PBBlock(
      Some(protobuf(header)),
      ByteString.copyFrom(block.signature.arr),
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
}
