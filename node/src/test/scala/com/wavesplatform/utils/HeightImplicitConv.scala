package com.wavesplatform.utils

import com.wavesplatform.state.Height

trait HeightImplicitConv {
  implicit def intToHeight(i: Int)                       = Height @@ i
  implicit def featuresMapWithHeight(m: Map[Short, Int]) = m.mapValues(intToHeight)
}
