package com.wavesplatform.transaction.protobuf
import com.wavesplatform.transaction.TransactionParserFor

import scala.util.Try

trait PBTransactionParser
    extends TransactionParserFor[com.wavesplatform.transaction.protobuf.Transaction]
    with com.wavesplatform.transaction.TransactionParser.OneVersion {

  val Transaction = com.wavesplatform.transaction.protobuf.Transaction

  override val version: Byte = 1
  override val typeId: Byte  = 0xff.toByte

  override protected def parseTail(bytes: Array[Byte]): Try[Transaction] = {
    Try(Transaction.parseFrom(bytes))
  }
}
