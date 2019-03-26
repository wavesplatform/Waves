package com.wavesplatform.generator.utils

import com.wavesplatform.account.AccountKeyPair
import com.wavesplatform.common.state.ByteStr

object Universe {
  var AccountsWithBalances: List[(AccountKeyPair, Long)] = Nil
  var IssuedAssets: List[ByteStr]                           = Nil
  var Leases: List[ByteStr]                                 = Nil
}
