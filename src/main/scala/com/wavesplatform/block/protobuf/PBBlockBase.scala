package com.wavesplatform.block.protobuf
import monix.eval.Coeval

trait PBBlockBase { block: Block =>
  val protoBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBBlockSerialiation.bytes(this))
  val protoBytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(PBBlockSerialiation.unsignedBytes(this))

  override def toString: String =
    s"Block(${header.signature} -> ${header.reference.trim}, txs=${transactions.size}, features=${header.featureVotes})"
}
