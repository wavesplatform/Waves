package com.wavesplatform.transaction.serialization.impl

import com.google.common.primitives.Bytes
import com.wavesplatform.protobuf.transaction
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.transaction.{Transaction, TransactionManifest, VersionedTransaction}

object PBTransactionSerializer {
  def bodyBytes(tx: Transaction): Array[Byte] =
    PBUtils.encodeDeterministic(PBTransactions.protobuf(tx).getTransaction)

  def toBytes(tx: Transaction): Array[Byte] =
    PBUtils.encodeDeterministic(PBTransactions.protobuf(tx))

  def toBytesPrefixed(tx: Transaction with VersionedTransaction): Array[Byte] =
    Bytes.concat(Array(0, tx.builder.typeId, tx.version), toBytes(tx))

  def fromBytes(bytes: Array[Byte]): Transaction =
    PBTransactions.vanilla(transaction.PBSignedTransaction.parseFrom(bytes)).fold(err => throw new IllegalArgumentException(err.toString), identity)

  def fromBytesAs(bytes: Array[Byte], builder: TransactionManifest): builder.TransactionT =
    fromBytes(bytes).asInstanceOf[builder.TransactionT]
}
