package com.wavesplatform.ride.runner

import com.wavesplatform.state.Height

import scala.util.NotGiven

package object db {
  type Heights = Vector[Height]
  val EmptyHeights: Heights = Vector.empty

  type =:!=[A, B] = NotGiven[A =:= B]
  type <:!<[A, B] = NotGiven[A <:< B]
}
