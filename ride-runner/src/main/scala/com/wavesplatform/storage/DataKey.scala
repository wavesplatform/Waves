package com.wavesplatform.storage

import com.wavesplatform.blockchain.SharedBlockchainStorage

trait DataKey extends Product with Serializable {
  type Value
  def reload[TagT](blockchainStorage: SharedBlockchainStorage[TagT], height: Int): Unit
}
