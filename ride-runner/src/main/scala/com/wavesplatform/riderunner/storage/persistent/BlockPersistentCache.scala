package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.block.SignedBlockHeader

trait BlockPersistentCache {
  def getLastHeight: Option[Int]
  def get(height: Int): Option[SignedBlockHeader]
  def getFrom(height: Int, n: Int): List[SignedBlockHeader]
  def set(height: Int, data: SignedBlockHeader): Unit
  def removeFrom(height: Int): Unit
}
