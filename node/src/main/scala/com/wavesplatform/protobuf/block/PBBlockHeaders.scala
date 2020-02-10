package com.wavesplatform.protobuf.block

import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.block

object PBBlockHeaders {

  def protobuf(header: VanillaBlockHeader): PBBlockHeader = {
    import header._

    new block.PBBlock.Header(
      AddressScheme.current.chainId,
      ByteString.copyFrom(reference),
      baseTarget,
      ByteString.copyFrom(generationSignature),
      featureVotes.map(shortToInt),
      timestamp,
      version,
      ByteString.copyFrom(generator),
      rewardVote,
      ByteString.copyFrom(transactionsRoot)
    )
  }

  def vanilla(header: PBBlockHeader): BlockHeader =
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

  private[this] def intToShort(int: Int): Short = {
    require(int >= 0 && int <= 65535, s"Short overflow: $int")
    int.toShort
  }

  private[this] def shortToInt(s: Short): Int = {
    java.lang.Short.toUnsignedInt(s)
  }
}
