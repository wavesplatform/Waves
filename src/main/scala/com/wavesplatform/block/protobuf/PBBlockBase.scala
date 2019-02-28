package com.wavesplatform.block.protobuf
import com.wavesplatform.serialization.protobuf.utils.PBWeakRefCacheSerializable

trait PBBlockBase extends PBWeakRefCacheSerializable { block: Block =>
  override def computeProtoBytes = PBBlockSerialization.signedBytes(this)
  override def computeProtoBytesUnsigned = PBBlockSerialization.unsignedBytes(this)

  override def toString: String =
    s"Block(${header.signature} -> ${header.reference.trim}, txs=${transactions.size}, features=${header.featureVotes})"
}
