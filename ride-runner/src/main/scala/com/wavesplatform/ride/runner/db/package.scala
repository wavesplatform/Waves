package com.wavesplatform.ride.runner

import com.wavesplatform.state.Height

package object db {
  type Heights = Vector[Height]
  val EmptyHeights: Heights = Vector.empty
}
