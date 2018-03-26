package com.wavesplatform.lang

import com.wavesplatform.lang.jvm.{Crypto, Base58}
import com.wavesplatform.lang.traits.Environment

abstract class WavesContext extends WavesContextImpl with Crypto with Environment with Base58 {

}
