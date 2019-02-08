package com.wavesplatform.transaction.protobuf
import com.wavesplatform.transaction.TransactionParser

import scala.reflect.ClassTag
import scala.util.Try

trait PBTransactionParser extends TransactionParser.OneVersion {
  def classTag: ClassTag[TransactionT] = ClassTag(com.wavesplatform.transaction.protobuf.Transaction.getClass)
  override type TransactionT = com.wavesplatform.transaction.protobuf.Transaction
  val Transaction: com.wavesplatform.transaction.protobuf.Transaction.type = com.wavesplatform.transaction.protobuf.Transaction

  override val version: Byte = 1
  override val typeId: Byte  = 0xff.toByte

  override protected def parseTail(bytes: Array[Byte]): Try[Transaction] = {
    Try(Transaction.parseFrom(bytes))
  }
}
