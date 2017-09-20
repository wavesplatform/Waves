package com.wavesplatform.general

import com.wavesplatform.settings.WavesSettings

object BlockVersion {
  def resolve(height: Int, settings: WavesSettings): Byte =
    if (height < settings.blockchainSettings.functionalitySettings.blockVersion3After) 2 else 3
}
