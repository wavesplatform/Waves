package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import monix.eval.Coeval

trait FastHashId extends Proven {
  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
}
