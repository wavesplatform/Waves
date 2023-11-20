package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait BlockDiskCache {
  def getLastHeight(implicit ctx: ReadOnly): Option[Height]
  def get(height: Height)(implicit ctx: ReadOnly): Option[SignedBlockHeaderWithVrf]
  def getFrom(height: Height, n: Int)(implicit ctx: ReadOnly): List[SignedBlockHeaderWithVrf]
  def set(height: Height, data: SignedBlockHeaderWithVrf)(implicit ctx: ReadWrite): Unit
  def setLastHeight(height: Height)(implicit ctx: ReadWrite): Unit
  def removeFrom(height: Height)(implicit ctx: ReadWrite): Unit
}
