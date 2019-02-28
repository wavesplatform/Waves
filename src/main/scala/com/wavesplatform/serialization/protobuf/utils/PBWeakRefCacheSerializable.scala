package com.wavesplatform.serialization.protobuf.utils
import scala.ref.WeakReference

trait PBWeakRefCacheSerializable {
  @transient private[this] var protoBytesCache = WeakReference[Array[Byte]](null)
  @transient private[this] var protoUnsignedBytesCache = WeakReference[Array[Byte]](null)

  private[wavesplatform] def computeProtoBytes: Array[Byte]
  private[wavesplatform] def computeProtoBytesUnsigned: Array[Byte]

  private[wavesplatform] def getOrComputeProtoBytes = {
    var bs = this.protoBytesCache.get
    if (bs.isEmpty) {
      bs = Some(computeProtoBytes)
      this.protoBytesCache = WeakReference(bs.get)
    }
    bs.get
  }

  private[wavesplatform] def getOrComputeProtoBytesUnsigned = {
    var bs = this.protoUnsignedBytesCache.get
    if (bs.isEmpty) {
      bs = Some(computeProtoBytesUnsigned)
      this.protoUnsignedBytesCache = WeakReference(bs.get)
    }
    bs.get
  }
}
