package com.wavesplatform.network

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.state.EndorsementStorage.EndorsementFilter
import com.wavesplatform.state.{EndorsementStorage, Height}
import com.wavesplatform.test.{FreeSpec, produce}
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.EitherValues

import java.util.concurrent.ThreadLocalRandom

class EndorsementStorageSpec extends FreeSpec with EitherValues {
  private val activeGenerator     = BlsKeyPair(TxHelpers.signer(0).privateKey)
  private val committedGenerator  = BlsKeyPair(TxHelpers.signer(1).privateKey)
  private val activeEndorserIndex = 1
  private val finalizedId         = mkRandomBlockId
  private val finalizedHeight     = Height(5)
  private val endorsedId          = mkRandomBlockId

  "tryAddVote" - {
    "rebroadcast if valid" in {
      val s = started

      info("on endorsement")
      val endorsement1 = BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, finalizedHeight, endorsedId)
      s.tryAddVote(EndorseBlock.from(endorsement1)).value shouldBe true
    }

    "ignore if" - {
      "an endorsement with" - {
        def test(msg: EndorseBlock, error: String): Unit = started.tryAddVote(msg) should produce(error)

        "a wrong signature" in test(
          EndorseBlock(activeEndorserIndex, finalizedId, finalizedHeight, endorsedId, ByteStr.empty),
          "Invalid signature"
        )

        "an unexpected height" in test(
          EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, Height(Int.MaxValue), endorsedId)),
          "Expected finalized height"
        )

        "an unexpected endorser" in test(
          EndorseBlock.from(BlockEndorsement.full(committedGenerator, 2, finalizedId, finalizedHeight, endorsedId)),
          "There are only"
        )

        "an already finalized block" in test(
          EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, finalizedHeight, finalizedId)),
          "Expected block"
        )
      }

      "already seen" in {
        val s = started

        info("on endorsement")
        val endorsement1 = BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, finalizedHeight, endorsedId)
        s.tryAddVote(EndorseBlock.from(endorsement1))

        info("on same endorsement")
        s.tryAddVote(EndorseBlock.from(endorsement1)).value shouldBe false
      }
    }
  }

  "tryCollectAndClear" - {
    "returns None if no updates" in {
      val s = started

      info("on start")
      s.tryCollectAndClear(endorsedId) shouldBe empty

      info("on endorsement")
      val endorsement1 = BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, finalizedHeight, endorsedId)
      s.tryAddVote(EndorseBlock.from(endorsement1))
      s.tryCollectAndClear(endorsedId) should not be empty

      info("on same endorsement")
      s.tryAddVote(EndorseBlock.from(endorsement1))
      s.tryCollectAndClear(endorsedId) shouldBe empty

      info("no new endorsements")
      s.tryCollectAndClear(endorsedId) shouldBe empty
    }
  }

  private def started: EndorsementStorage = {
    val r = new EndorsementStorage.InMemory
    r.startVoting(
      EndorsementFilter(
        miner = false,
        finalizedId,
        finalizedHeight,
        endorsedId,
        expectedEndorsers = Vector(committedGenerator.publicKey, activeGenerator.publicKey)
      )
    ) shouldBe true
    r
  }

  private def mkRandomBlockId: BlockId = ByteStr(Array.fill(SignatureLength)(ThreadLocalRandom.current().nextInt(Byte.MaxValue).toByte))
}
