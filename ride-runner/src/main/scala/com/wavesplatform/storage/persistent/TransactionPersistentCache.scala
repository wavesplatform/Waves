package com.wavesplatform.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.state.{Height, TransactionId}

trait TransactionPersistentCache {
  def getHeight(txId: TransactionId): RemoteData[Height]
  def setHeight(txId: TransactionId, height: RemoteData[Height]): Unit
  def remove(txId: TransactionId): Unit
}
