package com.wavesplatform.generator.utils

import com.wavesplatform.account.KeyPair
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction

object Universe {
  var AccountsWithBalances: List[(KeyPair, Long)] = Nil
  var IssuedAssets: List[IssueTransaction]        = Nil
  var Leases: List[LeaseTransaction]              = Nil
}
