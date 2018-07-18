package com.wavesplatform.transaction

trait VersionedTransaction {
  def version: Byte
}
