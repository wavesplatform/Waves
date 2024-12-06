package com.wavesplatform.utx

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.StateSnapshot
import com.wavesplatform.transaction.Transaction

final class UtxPriorityPool {

  @volatile private var priorityTxIds = Seq.empty[ByteStr]

  def priorityTransactionIds: Seq[ByteStr] = priorityTxIds

  private[utx] def setPriorityDiffs(discDiffs: Seq[StateSnapshot]): Set[Transaction] = {
    priorityTxIds = discDiffs.flatMap(_.transactions.keys)
    discDiffs.flatMap(_.transactions.values.map(_.transaction)).toSet
  }
}
