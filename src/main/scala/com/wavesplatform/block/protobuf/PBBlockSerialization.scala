package com.wavesplatform.block.protobuf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.protobuf.utils.PBUtils

private[block] object PBBlockSerialization {
  def signedBytes(block: PBBlock): Array[Byte] = {
    PBUtils.encodeDeterministic(block)
  }

  def unsignedBytes(block: PBBlock): Array[Byte] = {
    PBUtils.encodeDeterministic(block.withHeader(block.header.withSignature(ByteStr.empty)))
  }
}
