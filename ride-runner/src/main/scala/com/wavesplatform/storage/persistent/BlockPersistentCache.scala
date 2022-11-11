package com.wavesplatform.storage.persistent

import com.wavesplatform.block.SignedBlockHeader

trait BlockPersistentCache {
  def getLastHeight: Option[Int]
  def get(height: Int): Option[SignedBlockHeader]
  def set(height: Int, data: SignedBlockHeader): Unit
  def remove(fromHeight: Int): Unit
}
