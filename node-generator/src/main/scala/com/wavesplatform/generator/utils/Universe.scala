package com.wavesplatform.generator.utils

import com.wavesplatform.generator.Preconditions.CreatedAccount
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction

object Universe {
  @volatile var Accounts: List[CreatedAccount]       = Nil
  @volatile var IssuedAssets: List[IssueTransaction] = Nil
  @volatile var Leases: List[LeaseTransaction]       = Nil
}
