package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.ride.runner.storage.RemoteData

trait VrfPersistentCache {
  def get(height: Int)(implicit ctx: ReadOnly): RemoteData[ByteStr]
  def set(height: Int, vrf: Option[ByteStr])(implicit ctx: ReadWrite): Unit
  def removeFrom(height: Int)(implicit ctx: ReadWrite): Unit
}
