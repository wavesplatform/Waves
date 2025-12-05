package com.wavesplatform.finalization

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.{BalanceSnapshot, ConflictGenerators, GeneratorIndex, GenesisBlockHeight, Height}
import com.wavesplatform.test.{FreeSpec, WithResourceManager}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import org.scalatest.EitherValues

trait BaseFinalizationSpec extends FreeSpec, WithDomain, WithResourceManager, EitherValues {
  protected def mkConflictGenerators(h: Int, idxs: Int*): ConflictGenerators =
    ConflictGenerators.empty.appendAll(Height(h), GeneratorIndex.seq(idxs)*)

  protected def mkConflictVoting(conflict: BlockEndorsement*): FinalizationVoting = FinalizationVoting(
    valid = Nil,
    finalizedHeight = Height(0),
    aggregatedEndorsement = BlsSignature.Empty,
    conflict = conflict.toIndexedSeq
  )

  protected def mkConflictEndorsement(
      wavesAcc: KeyPair,
      idx: GeneratorIndex,
      endorsedBlock: Block,
      finalizedHeight: Height = GenesisBlockHeight
  ): BlockEndorsement = {
    val otherFinalizedBlockId = TxHelpers.randomBlockId
    BlockEndorsement.signed(
      BlsKeyPair(wavesAcc.privateKey),
      idx,
      otherFinalizedBlockId,
      finalizedHeight = finalizedHeight,
      endorsedId = endorsedBlock.id()
    )
  }

  protected def bs(height: Int, regularBalance: Long, deposits: Int = 0): BalanceSnapshot =
    BalanceSnapshot(Height(height), regularBalance, 0L, 0L, CommitToGenerationTransaction.DepositInWavelets * deposits)
}
