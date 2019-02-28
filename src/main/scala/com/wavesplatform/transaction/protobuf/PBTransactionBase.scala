package com.wavesplatform.transaction.protobuf
import com.wavesplatform.serialization.protobuf.utils.PBWeakRefCacheSerializable

trait PBTransactionBase extends PBWeakRefCacheSerializable { tx: PBTransaction =>
  override def computeProtoBytes = PBTransactionSerialization.signedBytes(PBSignedTransaction(this))
  override def computeProtoBytesUnsigned = PBTransactionSerialization.unsignedBytes(this)
}