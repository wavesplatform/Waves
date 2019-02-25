package com.wavesplatform.block.protobuf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.protobuf.utils.PBUtils
import monix.eval.Coeval

trait PBBlockBase { block: Block =>
  val protoBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    block.toByteArray
  }

  val protoBytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce {
    PBUtils.encodeDeterministic(block.withHeader(block.header.withSignature(ByteStr.empty)))
  }
}
