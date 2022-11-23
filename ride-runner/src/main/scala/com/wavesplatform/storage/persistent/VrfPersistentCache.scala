package com.wavesplatform.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.common.state.ByteStr

trait VrfPersistentCache {
  def get(height: Int): RemoteData[ByteStr]
  def set(height: Int, vrf: Option[ByteStr]): Unit
  def removeFrom(height: Int): Unit
}
