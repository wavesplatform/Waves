package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.BannedGenerator
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait BannedGeneratorsDiskCache {
  def getFrom(height: Height)(implicit ctx: ReadOnly): Vector[BannedGenerator]
  def removeFrom(height: Height)(implicit ctx: ReadWrite): Unit
}
