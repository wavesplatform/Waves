package com.wavesplatform.finalization

import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.{BalanceSnapshot, ConflictGenerators, GeneratorIndex, Height}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.CommitToGenerationTransaction

trait BaseFinalizationSpec extends FreeSpec, WithDomain {
  protected def mkConflictGenerators(h: Int, idxs: Int*): ConflictGenerators =
    ConflictGenerators.empty.appendAll(Height(h), GeneratorIndex.seq(idxs)*)

  protected def bs(height: Int, regularBalance: Long, deposits: Int = 0): BalanceSnapshot =
    BalanceSnapshot(height, regularBalance, 0L, 0L, CommitToGenerationTransaction.DepositInWavelets * deposits)
}
