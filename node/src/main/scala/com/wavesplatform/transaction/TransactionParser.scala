package com.wavesplatform.transaction

import com.wavesplatform.transaction.validation.TxValidator

import scala.util.Try

trait TransactionParser {
  def typeId: TxType
  def supportedVersions: Set[TxVersion]
  def parseBytes(bytes: Array[Byte]): Try[Transaction]
  def validator: TxValidator[_]
}
