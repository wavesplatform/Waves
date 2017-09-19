package com.wavesplatform.general

import com.wavesplatform.settings.WavesSettings

trait BlockVersionResolver{
  def resolve(height: Int, settings: WavesSettings): Byte
}

object BlockVersionResolver extends BlockVersionResolver{
  override def resolve(height: Int, settings: WavesSettings): Byte =
    if (height < settings.blockchainSettings.functionalitySettings.blockVersion3After) 2 else 3
}