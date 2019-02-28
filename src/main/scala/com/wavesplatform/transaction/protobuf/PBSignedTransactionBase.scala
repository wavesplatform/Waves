package com.wavesplatform.transaction.protobuf
import com.wavesplatform.serialization.protobuf.utils.PBWeakRefCacheSerializable

trait PBSignedTransactionBase extends PBWeakRefCacheSerializable { tx: SignedTransaction =>
  override def computeProtoBytes = PBTransactionSerialization.signedBytes(this)
  override def computeProtoBytesUnsigned = PBTransactionSerialization.unsignedBytes(this.transaction)
}
