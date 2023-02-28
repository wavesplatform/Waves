package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.riderunner.storage.StorageContext.{ReadOnly, ReadWrite}
import com.wavesplatform.state.{Height, TransactionId}

trait TransactionPersistentCache {
  def getHeight(txId: TransactionId)(implicit ctx: ReadOnly): RemoteData[Height]
  def setHeight(txId: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def remove(txId: TransactionId)(implicit ctx: ReadWrite): Unit
}
