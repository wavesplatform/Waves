package com.wavesplatform.protobuf.block

import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
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
      PBBlockHeaders.vanilla(header),
      ByteStr(block.signature.toByteArray),
      transactions
    )
  }

  def protobuf(block: VanillaBlock): PBBlock = {
    import block._

    new PBBlock(
      Some(PBBlockHeaders.protobuf(header)),
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
}
