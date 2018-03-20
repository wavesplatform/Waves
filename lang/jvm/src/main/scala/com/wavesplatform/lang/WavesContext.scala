package com.wavesplatform.lang

import com.wavesplatform.lang.jvm.Crypto
import com.wavesplatform.lang.traits.Emulator

abstract class WavesContext extends WavesContextImpl with Crypto with Emulator {

}
