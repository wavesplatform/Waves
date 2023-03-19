package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.ride.runner.storage.RemoteData
import PersistentStorageContext.{ReadOnly, ReadWrite}
import com.wavesplatform.state.{Height, TransactionId}

trait TransactionPersistentCache {
  def getHeight(txId: TransactionId)(implicit ctx: ReadOnly): RemoteData[Height]
  def setHeight(txId: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def remove(txId: TransactionId)(implicit ctx: ReadWrite): Unit
}
