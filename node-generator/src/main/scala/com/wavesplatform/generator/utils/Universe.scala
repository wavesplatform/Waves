package com.wavesplatform.generator.utils

import com.wavesplatform.generator.Preconditions.CreatedAccount
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.lease.LeaseTransaction

object Universe {
  @volatile var Accounts: List[CreatedAccount]         = Nil
  @volatile var IssuedAssets: List[IssueTransactionV2] = Nil
  @volatile var Leases: List[LeaseTransaction]         = Nil
}
