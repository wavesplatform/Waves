package com.wavesplatform.transaction.protobuf
import com.wavesplatform.transaction.TransactionParser

import scala.reflect.ClassTag
import scala.util.Try

trait PBTransactionParser extends TransactionParser.OneVersion {
  lazy val classTag: ClassTag[TransactionT] = ClassTag(classOf[VanillaTransaction])
  override type TransactionT = VanillaTransaction

  override val version: Byte = 1
  override val typeId: Byte  = 0xff.toByte

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    PBTransactionFactory.create(PBSignedTransaction.parseFrom(bytes)).left.map(err => new IllegalArgumentException(err.toString)).toTry
  }
}

object PBTransactionParser extends PBTransactionParser
