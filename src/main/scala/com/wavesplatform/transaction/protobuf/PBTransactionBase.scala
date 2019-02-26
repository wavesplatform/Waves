package com.wavesplatform.transaction.protobuf
import monix.eval.Coeval

trait PBTransactionBase { tx: PBTransaction =>
  val protoBytes = Coeval.evalOnce(PBTransactionSerialization.bytesWithTypePrefix(this))
  val protoUnsignedBytes = Coeval.evalOnce(PBTransactionSerialization.unsignedBytes(this))
}
