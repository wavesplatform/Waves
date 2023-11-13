package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.{Height, TransactionId}

trait TransactionDiskCache {
  def getHeight(id: TransactionId)(implicit ctx: ReadOnly): RemoteData[Height]
  def setHeight(id: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def updateHeightIfExist(id: TransactionId, height: RemoteData[Height])(implicit ctx: ReadWrite): Unit
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): Seq[TransactionId]
}
