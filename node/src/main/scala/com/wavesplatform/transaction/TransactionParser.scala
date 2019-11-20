package com.wavesplatform.transaction

import scala.reflect.ClassTag
import scala.util.Try

trait TransactionManifest {
  type TransactionT <: Transaction

  def classTag: ClassTag[TransactionT]
  def typeId: TxType
  def supportedVersions: Set[TxVersion]
}

trait TransactionParser extends TransactionManifest {
  def parseBytes(bytes: Array[Byte]): Try[TransactionT]
}
