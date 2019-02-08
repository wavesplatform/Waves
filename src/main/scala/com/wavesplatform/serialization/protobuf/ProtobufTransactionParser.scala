package com.wavesplatform.serialization.protobuf
import com.wavesplatform.transaction.TransactionParserFor
import com.wavesplatform.transaction.protobuf.Transaction

import scala.util.Try

object ProtobufTransactionParser
    extends TransactionParserFor[com.wavesplatform.transaction.protobuf.Transaction]
    with com.wavesplatform.transaction.TransactionParser.OneVersion {

  override val version: Byte = 1
  override val typeId: Byte  = 0xff.toByte

  override protected def parseTail(bytes: Array[Byte]): Try[Transaction] = {
    Try(Transaction.parseFrom(bytes))
  }
}
