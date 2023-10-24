package com.wavesplatform.transaction

import com.wavesplatform.transaction.validation.TxValidator

import scala.util.Try

trait TransactionParser {
  type TransactionT <: Transaction with VersionedTransaction

  def typeId: TxType

  def parseBytes(bytes: Array[Byte]): Try[TransactionT]

  implicit def validator: TxValidator[TransactionT]
}
