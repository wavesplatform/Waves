package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}

trait BlockPersistentCache {
  def getLastHeight(implicit ctx: ReadOnly): Option[Int]
  def get(height: Int)(implicit ctx: ReadOnly): Option[SignedBlockHeader]
  def getFrom(height: Int, n: Int)(implicit ctx: ReadOnly): List[SignedBlockHeader]
  def set(height: Int, data: SignedBlockHeader)(implicit ctx: ReadWrite): Unit
  def removeFrom(height: Int)(implicit ctx: ReadWrite): Unit
}
