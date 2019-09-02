package com.wavesplatform.utils

import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.extensions.{ApiExtensions, ApiExtensionsImpl}

object Implicits {
  implicit def blockchainToApiExtensions(b: Blockchain): ApiExtensions = ApiExtensionsImpl(b)
}
