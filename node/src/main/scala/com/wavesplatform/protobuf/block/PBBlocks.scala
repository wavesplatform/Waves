package com.wavesplatform.protobuf.block

import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.Block.{Header => PBHeader}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.protobuf.transaction.TransactionWrapper.Transaction.WavesTransaction

import scala.util.Try

object PBBlocks {
  def vanilla(header: PBBlock.Header): BlockHeader =
    BlockHeader(
      header.version.toByte,
      header.timestamp,
      header.reference.toByteStr,
      header.baseTarget,
      header.generationSignature.toByteStr,
      header.generator.toPublicKey,
      header.featureVotes.map(_.toShort),
      header.rewardVote,
      header.transactionsRoot.toByteStr
    )

  def vanilla(block: PBBlock, unsafe: Boolean = false): Try[VanillaBlock] = Try {
    require(block.header.isDefined, "block header is missing")
    val header = block.getHeader
    val transactions =
      if (header.version < VanillaBlock.HybridBlockVersion)
        block.wavesTransactions.map(PBTransactions.vanilla(_, unsafe).explicitGet())
      else block.wrappedTransactions.map(PBTransactions.vanilla(_, unsafe).explicitGet())

    VanillaBlock(vanilla(header), block.signature.toByteStr, transactions)
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

    if (block.header.version < VanillaBlock.HybridBlockVersion) {
      new PBBlock(
        Some(protobuf(header)),
        ByteString.copyFrom(block.signature.arr),
        wavesTransactions = transactionData.map(PBTransactions.protobuf)
      )
    } else {
      new PBBlock(
        Some(protobuf(header)),
        ByteString.copyFrom(block.signature.arr),
        wrappedTransactions = transactionData.map(PBTransactions.wrapped)
      )
    }


  }

  def clearChainId(block: PBBlock): PBBlock = {
    block.update(
      _.header.chainId := 0,
      _.wavesTransactions.foreach(_.transaction.chainId := 0),
      _.wrappedTransactions.foreach(_.transaction.modify {
        case WavesTransaction(value) => WavesTransaction(value.update(_.transaction.chainId := 0))
        case other                   => other
      })
    )
  }

  def addChainId(block: PBBlock): PBBlock = {
    val chainId = AddressScheme.current.chainId

    block.update(
      _.header.chainId := chainId,
      _.wavesTransactions.foreach(_.transaction.chainId := chainId),
      _.wrappedTransactions.foreach(_.transaction.modify {
        case WavesTransaction(value) => WavesTransaction(value.update(_.transaction.chainId := chainId))
        case other                   => other
      })
    )
  }
}
