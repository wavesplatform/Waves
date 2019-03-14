package com.wavesplatform.protobuf.block
import com.wavesplatform.protobuf.transaction.PBTransactions

object PBMicroBlocks {
  import com.wavesplatform.common.state.ByteStr
  import com.wavesplatform.protobuf.utils.PBInternalImplicits._

  // @todo deserialization to vanilla

  def protobuf(microBlock: VanillaMicroBlock): PBSignedMicroBlock =
    new PBSignedMicroBlock(
      microBlock = Some(
        PBMicroBlock(
          version = microBlock.version,
          reference = microBlock.prevResBlockSig,
          updatedBlockSignature = microBlock.totalResBlockSig,
          senderPublicKey = ByteStr(microBlock.sender.publicKey),
          transactions = microBlock.transactionData.map(PBTransactions.protobuf)
        )
      ),
      signature = microBlock.signature
    )
}
