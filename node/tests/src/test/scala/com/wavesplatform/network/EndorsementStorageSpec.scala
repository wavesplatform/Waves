package com.wavesplatform.network

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey}
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
      val s = started()

      log.info("on endorsement")
      val endorsement1 = BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, finalizedHeight, endorsedId)
      s.tryAddVote(EndorseBlock.from(endorsement1)).value shouldBe true
    }

    "ignore if" - {
      "an endorsement with" - {
        def test(msg: EndorseBlock, error: String): Unit = started().tryAddVote(msg) should produce(error)

        "a wrong signature" in test(
          EndorseBlock(activeEndorserIndex, finalizedId, finalizedHeight, endorsedId, ByteStr.empty),
          "Invalid signature"
        )

        "an unexpected height" in test(
          EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, Height(Int.MaxValue), endorsedId)),
          "Expected finalized height"
        )

        "invalid index" in test(
          EndorseBlock(-1, finalizedId, finalizedHeight, endorsedId, ByteStr.empty),
          "Invalid endorser index"
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
        val s = started()

        log.info("on endorsement")
        val endorsement1 = BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, finalizedHeight, endorsedId)
        s.tryAddVote(EndorseBlock.from(endorsement1))

        log.info("on same endorsement")
        s.tryAddVote(EndorseBlock.from(endorsement1)).value shouldBe false
      }
    }
  }

  "tryCollectAndClear" - {
    "returns None if no updates" in {
      val s = started()

      log.info("after start")
      s.checkTryCollect(endorsedId)

      log.info("after endorsement")
      val endorsement1 = BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, finalizedHeight, endorsedId)
      s.tryAddVote(EndorseBlock.from(endorsement1))
      s.checkTryCollect(endorsedId, Seq(activeEndorserIndex))

      log.info("after same endorsement")
      s.tryAddVote(EndorseBlock.from(endorsement1))
      s.checkTryCollect(endorsedId)

      log.info("after new endorsements")
      s.checkTryCollect(endorsedId)
    }

    // TODO: tests for conflicts
    "returns an updated voting information" - {
      val generators = (0 to 3).map(i => BlsKeyPair(TxHelpers.signer(i).privateKey)) // miner is #3

      def addVote(s: EndorsementStorage, generatorIndex: Int): Unit = {
        val endorsement = BlockEndorsement.full(generators(generatorIndex), generatorIndex, finalizedId, finalizedHeight, endorsedId)
        s.tryAddVote(EndorseBlock.from(endorsement))
      }

      "if updated and None if reached 2/3 with miner" in {
        val s = started(isMiner = true, generators.map(_.publicKey))

        log.info("after endorsement #0")
        addVote(s, 0) // 0 and miner
        s.checkTryCollect(endorsedId, Seq(0))

        log.info("after endorsement #1")
        addVote(s, 1) // 0, 1 and miner, reached 2/3
        s.checkTryCollect(endorsedId, Seq(0, 1))

        log.info("after endorsement #2")
        addVote(s, 1) // 0, 1, 2 and miner, already reached 2/3
        s.checkTryCollect(endorsedId)
      }

      "returns if got 2/3 in first time" in {
        val s = started(isMiner = true, generators.map(_.publicKey))

        log.info("after endorsement #0 and #1")
        addVote(s, 0) // 0 and miner
        addVote(s, 1) // 0, 1 and miner, reached 2/3
        s.checkTryCollect(endorsedId, Seq(0, 1))

        log.info("no new endorsements")
        s.checkTryCollect(endorsedId)
      }
    }
  }

  private def started(
      isMiner: Boolean = false,
      endorsers: Seq[BlsPublicKey] = Seq(committedGenerator.publicKey, activeGenerator.publicKey)
  ): EndorsementStorage = {
    val r = new EndorsementStorage.InMemory
    r.startVoting(EndorsementFilter(miner = isMiner, finalizedId, finalizedHeight, endorsedId, endorsers)) shouldBe true
    r
  }

  private def mkRandomBlockId: BlockId = ByteStr(Array.fill(SignatureLength)(ThreadLocalRandom.current().nextInt(Byte.MaxValue).toByte))

  extension (s: EndorsementStorage) {
    def checkTryCollect(endorsedId: BlockId, endorserIndexes: Seq[Int] = Nil): Unit = {
      val xs = s.tryCollectAndClear(endorsedId)
      xs.fold(Nil)(_.endorserIndexes) should contain theSameElementsInOrderAs endorserIndexes
    }
  }
}
