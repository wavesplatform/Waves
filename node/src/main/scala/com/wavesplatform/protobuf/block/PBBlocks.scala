package com.wavesplatform.protobuf.block

import scala.util.Try
import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.ByteStrExt
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.Block.Header as PBHeader
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction

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
      header.transactionsRoot.toByteStr,
      Option.unless(header.stateHash.isEmpty)(header.stateHash.toByteStr)
    )

  def vanilla(block: PBBlock, unsafe: Boolean = false): Try[VanillaBlock] = Try {
    require(block.header.isDefined, "block header is missing")
    VanillaBlock(vanilla(block.getHeader), block.signature.toByteStr, block.transactions.map(PBTransactions.vanilla(_, unsafe).explicitGet()))
  }

  def protobuf(header: BlockHeader): PBHeader = PBBlock.Header(
    AddressScheme.current.chainId,
    header.reference.toByteString,
    header.baseTarget,
    header.generationSignature.toByteString,
    header.featureVotes.map(_.toInt),
    header.timestamp,
    header.version,
    ByteString.copyFrom(header.generator.arr),
    header.rewardVote,
    header.transactionsRoot.toByteString,
    header.stateHash.getOrElse(ByteStr.empty).toByteString
  )

  def protobuf(block: VanillaBlock): PBBlock = {
    import block.*

    new PBBlock(
      Some(protobuf(header)),
      ByteString.copyFrom(block.signature.arr),
      transactionData.map(PBTransactions.protobuf)
    )
  }

  def clearChainId(block: PBBlock): PBBlock =
    block.update(
      _.header.chainId := 0,
      _.transactions.foreach(_.transaction.modify {
        case Transaction.WavesTransaction(value) => Transaction.WavesTransaction(value.update(_.chainId := 0))
        case other                               => other
      })
    )

  def addChainId(block: PBBlock): PBBlock = {
    val chainId = AddressScheme.current.chainId

    block.update(
      _.header.chainId := chainId,
      _.transactions.foreach(_.transaction.modify {
        case Transaction.WavesTransaction(value) => Transaction.WavesTransaction(value.update(_.chainId := chainId))
        case other                               => other
      })
    )
  }
}
