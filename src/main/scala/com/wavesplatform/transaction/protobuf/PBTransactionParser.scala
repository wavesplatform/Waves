package com.wavesplatform.transaction.protobuf
import com.wavesplatform.serialization.protobuf.PBImplicits._
import com.wavesplatform.transaction.TransactionParser

import scala.reflect.ClassTag
import scala.util.Try

trait PBTransactionParser extends TransactionParser.OneVersion {
  lazy val classTag: ClassTag[TransactionT] = ClassTag(classOf[PBSignedTransactionVanillaAdapter])
  override type TransactionT = PBSignedTransactionVanillaAdapter

  override val version: Byte = 1
  override val typeId: Byte  = 0xff.toByte

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try(PBSignedTransaction.parseFrom(bytes).toVanillaAdapter)
  }
}

object PBTransactionParser extends PBTransactionParser
