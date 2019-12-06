package com.wavesplatform.transaction

import scala.util.Try

trait TransactionParser {
  def typeId: TxType

  def supportedVersions: Set[TxVersion]

  def parseBytes(bytes: Array[Byte]): Try[Transaction]
}
