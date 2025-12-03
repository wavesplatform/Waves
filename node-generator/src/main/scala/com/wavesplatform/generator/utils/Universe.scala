package com.wavesplatform.generator.utils

import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction

object Universe {
  @volatile var IssuedAssets: List[IssueTransaction] = Nil
  @volatile var Leases: List[LeaseTransaction]       = Nil
}
