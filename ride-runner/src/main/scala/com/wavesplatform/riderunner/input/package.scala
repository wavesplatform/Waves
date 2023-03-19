package com.wavesplatform.riderunner

import com.wavesplatform.common.state.ByteStr

import java.nio.charset.StandardCharsets

package object input {
  // TODO #14
  def decodeStringLikeBytes(x: String): ByteStr = {
    if (x.startsWith("base64:") || x.startsWith("base58:")) ByteStr.decodeBase58(x.drop(7)).get
    else ByteStr(x.getBytes(StandardCharsets.UTF_8))
  }
}
