package com.wavesplatform.generator.utils

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr

object Universe {
  var AccountsWithBalances: List[(KeyPair, Long)] = Nil
  var IssuedAssets: List[ByteStr]                           = Nil
  var Leases: List[ByteStr]                                 = Nil
}
