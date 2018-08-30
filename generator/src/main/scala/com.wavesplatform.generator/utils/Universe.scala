package com.wavesplatform.generator.utils

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.state.ByteStr

object Universe {
  var AccountsWithBalances: List[(PrivateKeyAccount, Long)] = Nil
  var IssuedAssets: List[ByteStr]                           = Nil
  var Leases: List[ByteStr]                                 = Nil
}
