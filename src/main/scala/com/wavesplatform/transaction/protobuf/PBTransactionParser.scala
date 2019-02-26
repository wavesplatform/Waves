package com.wavesplatform.transaction.protobuf
import com.wavesplatform.transaction.TransactionParser

import scala.reflect.ClassTag
import scala.util.Try

trait PBTransactionParser extends TransactionParser.OneVersion {
  lazy val classTag: ClassTag[TransactionT] = ClassTag(classOf[com.wavesplatform.transaction.protobuf.PBTransaction.PBTransactionVanillaAdapter])
  override type TransactionT = com.wavesplatform.transaction.protobuf.PBTransaction.PBTransactionVanillaAdapter

  override val version: Byte = 1
  override val typeId: Byte  = 0xff.toByte

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try(Transaction.parseFrom(bytes))
  }
}

// object PBTransactionParser extends PBTransactionParser
