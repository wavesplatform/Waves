package com.wavesplatform.ride

import com.wavesplatform.account.PublicKey

package object input {
  val EmptyPublicKey = PublicKey(new Array[Byte](32))
}
