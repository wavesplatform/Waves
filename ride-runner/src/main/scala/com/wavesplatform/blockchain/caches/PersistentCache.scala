package com.wavesplatform.blockchain.caches

import com.wavesplatform.blockchain.BlockchainData

trait PersistentCache[KeyT, ValueT] {
  def get(maxHeight: Int, key: KeyT): BlockchainData[ValueT]
  def set(atHeight: Int, key: KeyT, data: BlockchainData[ValueT]): Unit
  def remove(fromHeight: Int, key: KeyT): BlockchainData[ValueT]
}

object PersistentCache {
  def empty[KeyT, ValueT]: PersistentCache[KeyT, ValueT] = new PersistentCache[KeyT, ValueT] {
    override def get(maxHeight: Int, key: KeyT): BlockchainData[ValueT]            = BlockchainData.Unknown
    override def set(atHeight: Int, key: KeyT, data: BlockchainData[ValueT]): Unit = {}
    override def remove(fromHeight: Int, key: KeyT): BlockchainData[ValueT]        = BlockchainData.Unknown
  }
}
