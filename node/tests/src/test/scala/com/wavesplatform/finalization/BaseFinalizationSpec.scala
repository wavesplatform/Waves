package com.wavesplatform.finalization

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain
import com.wavesplatform.state.{BalanceSnapshot, ConflictGenerators, GeneratorIndex, GenesisBlockHeight, Height}
import com.wavesplatform.test.{FreeSpec, WithResourceManager}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import org.scalactic.source.Position
import org.scalatest.EitherValues

trait BaseFinalizationSpec extends FreeSpec, WithDomain, WithResourceManager, EitherValues {
  protected def mkConflictGenerators(h: Int, idxs: Int*): ConflictGenerators =
    ConflictGenerators.empty.appendAll(Height(h), GeneratorIndex.seq(idxs)*)

  protected def mkFinalizationVoting(
      valid: Seq[GeneratorIndex] = Nil,
      finalizedHeight: Height = GenesisBlockHeight,
      conflict: Seq[BlockEndorsement] = Nil
  ): FinalizationVoting = FinalizationVoting(valid, finalizedHeight, aggregatedEndorsement = None, conflict)

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
      val aggSig = validEndorsers
        .map { kp =>
          BlockEndorsement.sign(
            BlsKeyPair(kp.privateKey),
            finalizedId = finalizedId,
            finalizedHeight = GenesisBlockHeight,
            endorsedId = endorsedId
          )
        }
        .foldLeft(Option.empty[BlsSignature]) {
          case (None, s)    => Some(s)
          case (Some(r), s) => Some(BlsSignature.agg(Seq(r, s)).value)
        }

      self.copy(aggregatedEndorsement = aggSig)
    }
  }

  extension (d: Domain)(using Position) {
    def finalizedHeightIsEmpty(): Domain = withClue("finalizedHeightIsEmpty: ") {
      d.blockchain.finalizedHeight shouldBe empty
      d
    }

    def finalizedHeightIs(h: Int): Domain = withClue("finalizedHeightIs: ") {
      d.blockchain.finalizedHeight.value.toInt shouldBe h
      d
    }

    def finalizedHeightAtPrevIsEmpty(): Domain = withClue("finalizedHeightAtIsEmpty: ") {
      val prevHeight = Height(d.blockchain.height - 1)
      if (prevHeight >= GenesisBlockHeight) d.blockchain.finalizedHeightAt(prevHeight) shouldBe empty
      d
    }

    def finalizedHeightAtPrevIs(h: Int): Domain = withClue("finalizedHeightAtIs: ") {
      val prevHeight = Height(d.blockchain.height - 1)
      d.blockchain.finalizedHeightAt(prevHeight).value.toInt shouldBe h
      d
    }

    def allFinalizedHeightIs(h: Int): Domain = d
      .finalizedHeightIs(h)
      .finalizedHeightAtPrevIs(h)
  }
}
