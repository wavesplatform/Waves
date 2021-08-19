package com.wavesplatform.protobuf.block

import scala.util.Try

import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.network.MicroBlockResponse
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.transaction.PBTransactions

object PBMicroBlocks {

  def vanilla(signedMicro: PBSignedMicroBlock, unsafe: Boolean = false): Try[MicroBlockResponse] = Try {
    require(signedMicro.microBlock.isDefined, "microblock is missing")
    val microBlock = signedMicro.getMicroBlock

    val transactions =
      if (microBlock.version < VanillaBlock.HybridBlockVersion)
        microBlock.wavesTransactions.map(PBTransactions.vanilla(_, unsafe).explicitGet())
      else microBlock.wrappedTransactions.map(PBTransactions.vanillaW(_, unsafe).explicitGet())

    MicroBlockResponse(
      VanillaMicroBlock(
        microBlock.version.toByte,
        PublicKey(microBlock.senderPublicKey.toByteArray),
        transactions,
        microBlock.reference.toByteStr,
        microBlock.updatedBlockSignature.toByteStr,
        signedMicro.signature.toByteStr
      ),
      signedMicro.totalBlockId.toByteStr
    )
  }

  def protobuf(microBlock: VanillaMicroBlock, totalBlockId: BlockId): PBSignedMicroBlock = {
    val template = PBMicroBlock(
      version = microBlock.version,
      reference = microBlock.reference.toByteString,
      updatedBlockSignature = microBlock.totalResBlockSig.toByteString,
      senderPublicKey = microBlock.sender.toByteString
    )

    val actualBlock = if (microBlock.version < VanillaBlock.HybridBlockVersion) {
      template.copy(wavesTransactions = microBlock.transactionData.map(PBTransactions.protobuf))
    } else {
      template.copy(wrappedTransactions = microBlock.transactionData.map(PBTransactions.wrapped))
    }

    new PBSignedMicroBlock(
      microBlock = Some(actualBlock),
      signature = microBlock.signature.toByteString,
      totalBlockId = totalBlockId.toByteString
    )
  }
}
