package com.wavesplatform.lang

import com.wavesplatform.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA
}
