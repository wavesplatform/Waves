package com.wavesplatform.protobuf.block
import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.utils.PBUtils

private[block] object PBBlockSerialization {
  def signedBytes(block: PBBlock): Array[Byte] = {
    PBUtils.encodeDeterministic(block)
  }

  def unsignedBytes(block: PBBlock): Array[Byte] = {
    PBUtils.encodeDeterministic(block.withHeader(block.getHeader.withSignature(ByteString.EMPTY)))
  }
}
