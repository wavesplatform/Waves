package com.wavesplatform.transaction.protobuf
import monix.eval.Coeval

trait PBSignedTransactionBase { tx: SignedTransaction =>
  val protoBytes         = Coeval.evalOnce(PBTransactionSerialization.signedBytes(this))
  val protoUnsignedBytes = Coeval.evalOnce(PBTransactionSerialization.unsignedBytes(this.transaction))
}
