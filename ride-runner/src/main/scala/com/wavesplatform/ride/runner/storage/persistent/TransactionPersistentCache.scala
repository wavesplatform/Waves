package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.state.{Height, TransactionId}

trait TransactionPersistentCache {
  def getAllKeys(fromHeight: Height)(implicit ctx: ReadOnly): Seq[TransactionId]
  def getHeight(txId: TransactionId)(implicit ctx: ReadOnly): RemoteData[Height]
  def setHeight(txId: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Seq[TransactionId]
}
