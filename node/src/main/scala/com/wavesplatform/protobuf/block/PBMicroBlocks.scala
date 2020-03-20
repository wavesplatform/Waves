package com.wavesplatform.protobuf.block

import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.PBTransactions

import scala.util.Try

object PBMicroBlocks {
  import com.wavesplatform.common.state.ByteStr
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  def vanilla(signedMicro: PBSignedMicroBlock, unsafe: Boolean = false): Try[(VanillaMicroBlock, BlockId)] = Try {
    require(signedMicro.microBlock.isDefined, "microblock is missing")
    val microBlock   = signedMicro.getMicroBlock
    val transactions = microBlock.transactions.map(PBTransactions.vanilla(_, unsafe).explicitGet())
    VanillaMicroBlock(
      microBlock.version.toByte,
      PublicKey(microBlock.senderPublicKey.toByteArray),
      transactions,
      microBlock.reference,
      microBlock.updatedBlockSignature,
      signedMicro.signature
    ) -> signedMicro.totalBlockId
  }

  def protobuf(microBlock: VanillaMicroBlock, totalBlockId: BlockId): PBSignedMicroBlock =
    new PBSignedMicroBlock(
      microBlock = Some(
        PBMicroBlock(
          version = microBlock.version,
          reference = microBlock.reference,
          updatedBlockSignature = microBlock.totalResBlockSig,
          senderPublicKey = ByteStr(microBlock.sender),
          transactions = microBlock.transactionData.map(PBTransactions.protobuf)
        )
      ),
      signature = microBlock.signature,
      totalBlockId = totalBlockId
    )
}
