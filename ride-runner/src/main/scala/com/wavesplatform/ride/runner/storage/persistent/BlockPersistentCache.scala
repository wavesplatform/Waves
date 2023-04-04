package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}

trait BlockPersistentCache {
  def getLastHeight(implicit ctx: ReadOnly): Option[Int]
  def get(height: Int)(implicit ctx: ReadOnly): Option[SignedBlockHeaderWithVrf]
  def getFrom(height: Int, n: Int)(implicit ctx: ReadOnly): List[SignedBlockHeaderWithVrf]
  def set(height: Int, data: SignedBlockHeaderWithVrf)(implicit ctx: ReadWrite): Unit
  def removeFrom(height: Int)(implicit ctx: ReadWrite): Unit
}
