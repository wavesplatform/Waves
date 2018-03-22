package com.wavesplatform.lang

import com.wavesplatform.lang.jvm.Crypto
import com.wavesplatform.lang.traits.Environment

abstract class WavesContext extends WavesContextImpl with Crypto with Environment {

}
