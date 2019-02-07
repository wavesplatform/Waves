package com.wavesplatform.serialization.protobuf
import com.wavesplatform.transaction.TransactionParserFor
import com.wavesplatform.transaction.protobuf.Transaction

import scala.util.Try

trait ProtobufTransactionParser
    extends TransactionParserFor[com.wavesplatform.transaction.protobuf.Transaction]
    with com.wavesplatform.transaction.TransactionParser.OneVersion {

  override val typeId: Byte                 = 0xff.toByte
  override val supportedVersions: Set[Byte] = Set(1)

  override protected def parseTail(bytes: Array[Byte]): Try[Transaction] = {
    Try(Transaction.parseFrom(bytes))
  }
}
