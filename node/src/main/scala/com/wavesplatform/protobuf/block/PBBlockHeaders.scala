package com.wavesplatform.protobuf.block

import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.ChainId

object PBBlockHeaders {
  def protobuf(header: VanillaBlockHeader): PBBlockHeader = {
    import header._

    PBBlock.Header(
      ChainId.global,
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
  }

  def vanilla(header: PBBlockHeader): VanillaBlockHeader =
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

  private[this] def shortToInt(s: Short): Int = {
    java.lang.Short.toUnsignedInt(s)
  }

  private[this] def intToShort(int: Int): Short = {
    require(int >= 0 && int <= 65535, s"Short overflow: $int")
    int.toShort
  }
}
