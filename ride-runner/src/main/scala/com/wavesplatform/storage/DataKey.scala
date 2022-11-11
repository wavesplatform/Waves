package com.wavesplatform.storage

trait DataKey {
  def reload(height: Int): Unit
}
