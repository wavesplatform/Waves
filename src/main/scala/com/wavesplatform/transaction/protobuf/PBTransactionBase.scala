package com.wavesplatform.transaction.protobuf
import monix.eval.Coeval

trait PBTransactionBase { tx: PBTransaction =>
  val protoBytes         = Coeval.evalOnce(PBTransactionSerialization.signedBytes(PBSignedTransaction(this)))
  val protoUnsignedBytes = Coeval.evalOnce(PBTransactionSerialization.unsignedBytes(this))
}
