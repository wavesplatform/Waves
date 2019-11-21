package com.wavesplatform.transaction.serialization.impl

import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.transaction.Transaction

object PBTransactionSerializer {
  def bodyBytes(tx: Transaction): Array[Byte] =
    PBUtils.encodeDeterministic(PBTransactions.protobuf(tx).getTransaction)
}
