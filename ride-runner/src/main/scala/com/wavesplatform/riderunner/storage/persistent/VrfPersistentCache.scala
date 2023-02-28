package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.riderunner.storage.StorageContext.{ReadOnly, ReadWrite}

trait VrfPersistentCache {
  def get(height: Int)(implicit ctx: ReadOnly): RemoteData[ByteStr]
  def set(height: Int, vrf: Option[ByteStr])(implicit ctx: ReadWrite): Unit
  def removeFrom(height: Int)(implicit ctx: ReadWrite): Unit
}
