package com.wavesplatform.network

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.state.EndorsementStorage.EndorsementFilter
import com.wavesplatform.state.{EndorsementStorage, Height}
import com.wavesplatform.test.{FreeSpec, NumericExt, produce}
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.EitherValues

import java.util.concurrent.ThreadLocalRandom

class EndorsementStorageSpec extends FreeSpec with EitherValues {
  private type GeneratorBalance = (blsKp: BlsKeyPair, balance: Long)

  private val activeGenerator       = BlsKeyPair(TxHelpers.signer(0).privateKey)
  private val committedGenerator    = BlsKeyPair(TxHelpers.signer(1).privateKey)
  private val activeEndorserIndex   = 1
  private val finalizedHeight       = Height(5)
  private val expectedFinalizedId   = mkRandomBlockId
  private val unexpectedFinalizedId = mkRandomBlockId
  private val endorsedId            = mkRandomBlockId

  "tryAddVote" - {
    "rebroadcast if valid" in {
      val s = started()

      log.info("on endorsement")
      val endorsement1 = BlockEndorsement.full(activeGenerator, activeEndorserIndex, expectedFinalizedId, finalizedHeight, endorsedId)
      s.tryAddVote(EndorseBlock.from(endorsement1)).value shouldBe true
    }

    "ignore if" - {
      "an endorsement with" - {
        def test(msg: EndorseBlock, error: String): Unit = started().tryAddVote(msg) should produce(error)

        "a wrong signature" in test(
          EndorseBlock(activeEndorserIndex, expectedFinalizedId, finalizedHeight, endorsedId, ByteStr.empty),
          "Invalid signature"
        )

        "an unexpected height" in test(
          EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, expectedFinalizedId, Height(Int.MaxValue), endorsedId)),
          "Expected finalized height"
        )

        "invalid index" in test(
          EndorseBlock(-1, expectedFinalizedId, finalizedHeight, endorsedId, ByteStr.empty),
          "Invalid endorser index"
        )

        "an unexpected endorser" in test(
          EndorseBlock.from(BlockEndorsement.full(committedGenerator, 2, expectedFinalizedId, finalizedHeight, endorsedId)),
          "There are only"
        )

        "an already finalized block" in test(
          EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, expectedFinalizedId, finalizedHeight, expectedFinalizedId)),
          "Expected block"
        )
      }

      "already seen" in {
        val s = started()

        log.info("on endorsement")
        val endorsement1 = BlockEndorsement.full(activeGenerator, activeEndorserIndex, expectedFinalizedId, finalizedHeight, endorsedId)
        s.tryAddVote(EndorseBlock.from(endorsement1))

        log.info("on same endorsement")
        s.tryAddVote(EndorseBlock.from(endorsement1)).value shouldBe false
      }
    }
  }

  "tryCollectAndClear" - {
    // miner is #3
    val generators: IndexedSeq[GeneratorBalance] = (0 to 3).map { i =>
      val blsKp = BlsKeyPair(TxHelpers.signer(i).privateKey)
      (blsKp, 100_000.waves + i)
    }

    extension (s: EndorsementStorage) {
      private def addValidVote(generatorIndex: Int): Either[String, Boolean] = {
        val endorsement = BlockEndorsement.full(generators(generatorIndex).blsKp, generatorIndex, expectedFinalizedId, finalizedHeight, endorsedId)
        s.tryAddVote(EndorseBlock.from(endorsement))
      }

      private def addConflictVote(generatorIndex: Int): Either[String, Boolean] = {
        val endorsement = BlockEndorsement.full(generators(generatorIndex).blsKp, generatorIndex, unexpectedFinalizedId, finalizedHeight, endorsedId)
        s.tryAddVote(EndorseBlock.from(endorsement))
      }

      private def checkTryCollect(endorsedId: BlockId, valid: Seq[Int] = Nil, conflict: Seq[Int] = Nil): Unit =
        s.tryCollectAndClear(endorsedId) match {
          case None if valid.nonEmpty || conflict.nonEmpty =>
            fail(s"Expected valid endorsers [${valid.mkString(", ")}], conflict endorsers [${conflict.mkString(", ")}], got None")
          case Some(v) =>
            withClue("valid: ") {
              v.endorserIndexes should contain theSameElementsAs valid
            }
            withClue("conflict: ") {
              v.conflict.map(_.endorserIndex) should contain theSameElementsAs conflict
            }
            v.aggregatedEndorsement match {
              case BlsSignature.Empty =>
                if (valid.nonEmpty) fail(s"Signature can't be empty if endorsers nonempty: [${valid.mkString(", ")}]")
              case aggEnd: BlsSignature.NonEmpty =>
                withClue("signature: ") {
                  aggEnd
                    .verifyAgg(
                      BlockEndorsement.mkMessage(expectedFinalizedId, finalizedHeight, endorsedId),
                      valid.map(generators(_).blsKp.publicKey)
                    )
                    .value shouldBe true
                }
            }
          case _ =>
        }
    }

    "returns None" - {
      // finalization means 2/3 with miner
      "if not reached finalization" in {
        val s = started(minerIndex = 3, generators)

        log.info("no endorsements")
        s.checkTryCollect(endorsedId)

        log.info("after endorsement #0")
        s.addValidVote(0) // 0 and miner
        s.checkTryCollect(endorsedId)
      }

      "on second request if we already reached finalization even we have a new valid vote" in {
        val s = started(minerIndex = 3, generators)

        log.info("reached")
        s.addValidVote(0)
        s.addValidVote(1) // 0, 1 and miner, reached finalization
        s.checkTryCollect(endorsedId, Seq(0, 1))

        log.info("second request")
        s.checkTryCollect(endorsedId)

        log.info("new vote")
        s.addValidVote(2)
        s.checkTryCollect(endorsedId)
      }
    }

    "returns Some" - {
      "with only either valid, or conflict vote from one endorser" - {
        "valid, then conflict" in {
          val s = started(minerIndex = 3, generators)

          s.addValidVote(2)
          s.addConflictVote(2)
          s.checkTryCollect(endorsedId, conflict = Seq(2))
        }

        "conflict, then valid" in {
          val s = started(minerIndex = 3, generators)

          s.addConflictVote(2)
          s.addValidVote(2)
          s.checkTryCollect(endorsedId, conflict = Seq(2))
        }
      }

      "when reached or lost finalization" in {
        val s = started(minerIndex = 3, generators)

        log.debug("reached finalization")
        s.addValidVote(0)
        s.addValidVote(1) // 0, 1 and miner, reached 2/3
        s.checkTryCollect(endorsedId, Seq(0, 1))

        log.debug("lost finalization, removes from valid")
        s.addConflictVote(0)
        s.checkTryCollect(endorsedId, valid = Seq(1), conflict = Seq(0))
      }

      "if have a new conflict vote" - {
        "even no valid votes" in {
          val s = started(minerIndex = 3, generators)

          s.addConflictVote(2)
          s.checkTryCollect(endorsedId, conflict = Seq(2))
        }

        "even insufficient valid votes" in {
          val s = started(minerIndex = 3, generators)

          s.addValidVote(0)
          s.addConflictVote(2)
          s.checkTryCollect(endorsedId, valid = Seq(0), conflict = Seq(2))
        }

        "if finalized" in {
          val s = started(minerIndex = 3, generators)

          s.addValidVote(0)
          s.addValidVote(1) // 0, 1 and miner, reached 2/3
          s.addConflictVote(2)
          s.checkTryCollect(endorsedId, valid = Seq(0, 1), conflict = Seq(2))
        }

        "after finalization" in {
          val s = started(minerIndex = 3, generators)

          s.addValidVote(0)
          s.addValidVote(1) // 0, 1 and miner, reached 2/3
          s.checkTryCollect(endorsedId, valid = Seq(0, 1))

          log.debug("after finalization")
          s.addConflictVote(2)
          s.checkTryCollect(endorsedId, valid = Seq(0, 1), conflict = Seq(2))
        }
      }
    }
  }

  private def started(
      minerIndex: Int = -1,
      endorsers: IndexedSeq[GeneratorBalance] = IndexedSeq(committedGenerator -> 100_000.waves, activeGenerator -> 100_000.waves)
  ): EndorsementStorage = {
    require(minerIndex == -1 || minerIndex >= 0 && minerIndex < endorsers.size, "Invalid miner index")
    val r = new EndorsementStorage.InMemory
    r.startVoting(
      EndorsementFilter(
        minerIndex = if (minerIndex < 0) None else Some(minerIndex),
        expectedFinalizedId,
        finalizedHeight,
        endorsedId,
        endorsers.map(x => (x.blsKp.publicKey, x.balance))
      )
    ) shouldBe true
    r
  }

  private def mkRandomBlockId: BlockId = ByteStr(Array.fill(SignatureLength)(ThreadLocalRandom.current().nextInt(Byte.MaxValue).toByte))
}
