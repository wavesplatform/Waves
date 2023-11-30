package com.wavesplatform.protobuf.block

import scala.util.Try
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.network.MicroBlockResponse
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.transaction.PBTransactions

object PBMicroBlocks {

  def vanilla(signedMicro: PBSignedMicroBlock, unsafe: Boolean = false): Try[MicroBlockResponse] = Try {
    require(signedMicro.microBlock.isDefined, "microblock is missing")
    val microBlock   = signedMicro.getMicroBlock
    val transactions = microBlock.transactions.map(PBTransactions.vanilla(_, unsafe).explicitGet())
    MicroBlockResponse(
      VanillaMicroBlock(
        microBlock.version.toByte,
        PublicKey(microBlock.senderPublicKey.toByteArray),
        transactions,
        microBlock.reference.toByteStr,
        microBlock.updatedBlockSignature.toByteStr,
        signedMicro.signature.toByteStr,
        Option.unless(microBlock.stateHash.isEmpty)(microBlock.stateHash.toByteStr)
      ),
      signedMicro.totalBlockId.toByteStr
    )
  }

  def protobuf(microBlock: VanillaMicroBlock, totalBlockId: BlockId): PBSignedMicroBlock =
    new PBSignedMicroBlock(
      microBlock = Some(
        PBMicroBlock(
          version = microBlock.version,
          reference = microBlock.reference.toByteString,
          updatedBlockSignature = microBlock.totalResBlockSig.toByteString,
          senderPublicKey = microBlock.sender.toByteString,
          transactions = microBlock.transactionData.map(PBTransactions.protobuf),
          stateHash = microBlock.stateHash.getOrElse(ByteStr.empty).toByteString
        )
      ),
      signature = microBlock.signature.toByteString,
      totalBlockId = totalBlockId.toByteString
    )
}
