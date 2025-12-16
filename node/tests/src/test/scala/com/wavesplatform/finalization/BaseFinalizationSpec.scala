package com.wavesplatform.finalization

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.{BalanceSnapshot, ConflictGenerators, GeneratorIndex, GenesisBlockHeight, Height}
import com.wavesplatform.test.{FreeSpec, WithResourceManager}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import org.scalatest.EitherValues

trait BaseFinalizationSpec extends FreeSpec, WithDomain, WithResourceManager, EitherValues {
  protected def mkConflictGenerators(h: Int, idxs: Int*): ConflictGenerators =
    ConflictGenerators.empty.appendAll(Height(h), GeneratorIndex.seq(idxs)*)

  protected def mkFinalizationVoting(
      valid: Seq[GeneratorIndex] = Nil,
      finalizedHeight: Height = GenesisBlockHeight,
      aggregatedEndorsement: BlsSignature = BlsSignature.Empty,
      conflict: Seq[BlockEndorsement] = Nil
  ): FinalizationVoting = FinalizationVoting(valid, finalizedHeight, aggregatedEndorsement, conflict)

  protected def mkConflictEndorsement(
      wavesAcc: KeyPair,
      idx: GeneratorIndex,
      endorsedId: BlockId,
      finalizedHeight: Height = GenesisBlockHeight,
      finalizedId: BlockId = TxHelpers.randomBlockId
  ): BlockEndorsement = BlockEndorsement.signed(
    BlsKeyPair(wavesAcc.privateKey),
    idx,
    finalizedId,
    finalizedHeight = finalizedHeight,
    endorsedId = endorsedId
  )

  protected def bs(height: Int, regularBalance: Long, deposits: Int = 0): BalanceSnapshot =
    BalanceSnapshot(Height(height), regularBalance, 0L, 0L, CommitToGenerationTransaction.DepositInWavelets * deposits)

  extension (self: FinalizationVoting) {
    def withConflict(
        wavesAcc: KeyPair,
        idx: GeneratorIndex,
        endorsedId: BlockId,
        finalizedHeight: Height = GenesisBlockHeight,
        finalizedId: BlockId = TxHelpers.randomBlockId
    ): FinalizationVoting = self.copy(conflict = self.conflict :+ mkConflictEndorsement(wavesAcc, idx, endorsedId, finalizedHeight, finalizedId))

    def signed(endorsedId: BlockId, finalizedId: BlockId, validEndorsers: KeyPair*): FinalizationVoting = {
      val aggSig = validEndorsers.foldLeft(BlsSignature.Empty: BlsSignature) { case (r, kp) =>
        val sig = BlockEndorsement.sign(
          BlsKeyPair(kp.privateKey),
          finalizedId = finalizedId,
          finalizedHeight = GenesisBlockHeight,
          endorsedId = endorsedId
        )
        r.append(sig)
      }

      self.copy(aggregatedEndorsement = aggSig)
    }
  }
}
