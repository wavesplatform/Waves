package com.wavesplatform.network

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.state.EndorsementStorage.EndorsementFilter
import com.wavesplatform.state.{EndorsementStorage, GeneratorIndex, Height}
import com.wavesplatform.test.{FreeSpec, NumericExt, produce}
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position
import org.scalatest.EitherValues

class EndorsementStorageSpec extends FreeSpec with EitherValues {
  private type GeneratorBalance = (blsKp: BlsKeyPair, balance: Long)

  private val committedGenerator = BlsKeyPair(TxHelpers.signer(1).privateKey) // GeneratorIndex(0)

  private val activeGenerator      = BlsKeyPair(TxHelpers.signer(0).privateKey)
  private val activeGeneratorIndex = GeneratorIndex(1)

  private val expectedFinalizedHeight = Height(5)

  private val expectedFinalizedId, unexpectedFinalizedId, expectedEndorsedId = TxHelpers.randomBlockId

  "tryAddVote" - {
    def mk(
        endorserAccount: BlsKeyPair = activeGenerator,
        endorserIndex: GeneratorIndex = activeGeneratorIndex,
        finalizedId: BlockId = expectedFinalizedId,
        finalizedHeight: Height = expectedFinalizedHeight,
        endorsedId: BlockId = expectedEndorsedId
    ): BlockEndorsement.Full = BlockEndorsement.full(endorserAccount, endorserIndex, finalizedId, finalizedHeight, endorsedId)

    "rebroadcast if" - {
      "valid" in {
        started().tryAddEndorsement(mk()).value shouldBe true
      }

      "conflict" - {
        "same finalized height" in {
          started().tryAddEndorsement(mk(finalizedId = unexpectedFinalizedId)).value shouldBe true
        }

        "smaller finalized height" in {
          started(hasSameBlockBeforeFinalizationHeight = false)
            .tryAddEndorsement(mk(finalizedId = unexpectedFinalizedId, finalizedHeight = Height(expectedFinalizedHeight - 1)))
            .value shouldBe true
        }
      }
    }

    "don't rebroadcast if miner" in {
      started(minerIndex = 1).tryAddEndorsement(mk()).value shouldBe false
    }

    "ignore if" - {
      "an endorsement with" - {
        def test(msg: EndorseBlock, error: String): Unit = started().tryAdd(msg) should produce(error)

        "a wrong signature" in test(
          EndorseBlock(activeGeneratorIndex.toInt, expectedFinalizedId, expectedFinalizedHeight, expectedEndorsedId, ByteStr.empty),
          "Invalid signature"
        )

        "an unexpected height" in test(
          EndorseBlock.from(mk(finalizedHeight = Height(Int.MaxValue))),
          "Expected finalized height"
        )

        "invalid index" in test(
          EndorseBlock(-1, expectedFinalizedId, expectedFinalizedHeight, expectedEndorsedId, ByteStr.empty),
          "Invalid endorser index"
        )

        "an unexpected endorser" in test(
          EndorseBlock.from(mk(committedGenerator, GeneratorIndex(2))),
          "There are only"
        )
      }

      "known as sender of conflict endorsements before" in {
        started(conflict = Set(activeGeneratorIndex)).tryAddEndorsement(mk()).value shouldBe false
      }

      "already seen" - {
        "valid" in {
          val s = started()

          log.info("on endorsement")
          val endorsement = mk()
          s.tryAddEndorsement(endorsement).value

          log.info("on same endorsement")
          s.tryAddEndorsement(endorsement).value shouldBe false
        }

        "conflict" in {
          val s = started()

          val endorsement = mk(finalizedId = unexpectedFinalizedId)
          s.tryAddEndorsement(endorsement).value
          s.tryAddEndorsement(endorsement).value shouldBe false
        }
      }

      "a second conflict endorsement from the same endorser" in {
        val s = started()

        s.tryAddEndorsement(mk(finalizedId = unexpectedFinalizedId)).value
        s.tryAddEndorsement(mk(finalizedId = TxHelpers.randomBlockId)).value shouldBe false
      }

      "a valid endorsement after conflict from the same endorser" in {
        val s = started()

        s.tryAddEndorsement(mk(finalizedId = unexpectedFinalizedId)).value
        s.tryAddEndorsement(mk()).value shouldBe false
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
      private def addValidVote(generatorIndex: Int): Either[String, Boolean]    = s.addVote(generatorIndex, expectedFinalizedId)
      private def addConflictVote(generatorIndex: Int): Either[String, Boolean] = s.addVote(generatorIndex, unexpectedFinalizedId)
      private def addVote(generatorIndex: Int, finalizedId: BlockId): Either[String, Boolean] = s.tryAddEndorsement(
        BlockEndorsement
          .full(generators(generatorIndex).blsKp, GeneratorIndex(generatorIndex), finalizedId, expectedFinalizedHeight, expectedEndorsedId)
      )

      private def checkTryCollect(endorsedId: BlockId, valid: Seq[Int] = Nil, conflict: Seq[Int] = Nil)(using Position): Unit =
        s.tryCollectAndClear(endorsedId) match {
          case None if valid.nonEmpty || conflict.nonEmpty =>
            fail(s"Expected valid endorsers [${valid.mkString(", ")}], conflict endorsers [${conflict.mkString(", ")}], got None")
          case Some(v) =>
            withClue("valid: ") {
              v.valid should contain theSameElementsAs valid
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
                      BlockEndorsement.mkMessage(expectedFinalizedId, expectedFinalizedHeight, endorsedId),
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
        s.checkTryCollect(expectedEndorsedId)

        log.info("after endorsement #0")
        s.addValidVote(0) // 0 and miner
        s.checkTryCollect(expectedEndorsedId)
      }

      "on second request if we already reached finalization even we have a new valid vote" in {
        val s = started(minerIndex = 3, generators)

        log.info("reached")
        s.addValidVote(0)
        s.addValidVote(1) // 0, 1 and miner, reached finalization
        s.checkTryCollect(expectedEndorsedId, Seq(0, 1))

        log.info("second request")
        s.checkTryCollect(expectedEndorsedId)

        log.info("new vote")
        s.addValidVote(2)
        s.checkTryCollect(expectedEndorsedId)
      }
    }

    "returns Some" - {
      "with only either valid, or conflict vote from one endorser" - {
        "valid, then conflict" in {
          val s = started(minerIndex = 3, generators)

          s.addValidVote(2)
          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(2))
        }

        "conflict, then valid" in {
          val s = started(minerIndex = 3, generators)

          s.addConflictVote(2)
          s.addValidVote(2)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(2))
        }
      }

      "when reached or lost finalization due to conflict vote" in {
        val s = started(minerIndex = 3, generators)

        log.debug("reached finalization")
        s.addValidVote(0)
        s.addValidVote(1) // 0, 1 and miner, reached 2/3
        s.checkTryCollect(expectedEndorsedId, Seq(0, 1))

        log.debug("lost finalization, removes from valid")
        s.addConflictVote(0)
        s.checkTryCollect(expectedEndorsedId, valid = Seq(1), conflict = Seq(0))
      }

      "when got a new conflict vote" - {
        "even no valid votes" in {
          val s = started(minerIndex = 3, generators)

          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(2))
        }

        "even insufficient valid votes" in {
          val s = started(minerIndex = 3, generators)

          s.addValidVote(0)
          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, valid = Seq(0), conflict = Seq(2)) // TODO: no valid!
        }

        "if finalized" in {
          val s = started(minerIndex = 3, generators)

          s.addValidVote(0)
          s.addValidVote(1) // 0, 1 and miner, reached 2/3
          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, valid = Seq(0, 1), conflict = Seq(2))
        }

        "after finalization" in {
          val s = started(minerIndex = 3, generators)

          s.addValidVote(0)
          s.addValidVote(1) // 0, 1 and miner, reached 2/3
          s.checkTryCollect(expectedEndorsedId, valid = Seq(0, 1))

          log.debug("after finalization")
          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, valid = Seq(0, 1), conflict = Seq(2))
        }
      }
    }
  }

  private def started(
      minerIndex: Int = -1,
      endorsers: IndexedSeq[GeneratorBalance] = IndexedSeq(committedGenerator -> 100_000.waves, activeGenerator -> 100_000.waves),
      conflict: Set[GeneratorIndex] = Set.empty,
      hasSameBlockBeforeFinalizationHeight: Boolean = true
  ): EndorsementStorage = {
    require(minerIndex == -1 || minerIndex >= 0 && minerIndex < endorsers.size, "Invalid miner index")
    val r = new EndorsementStorage.InMemory((_, _) => hasSameBlockBeforeFinalizationHeight)
    r.startVoting(
      EndorsementFilter(
        GeneratorIndex.checked(minerIndex),
        expectedFinalizedId,
        expectedFinalizedHeight,
        expectedEndorsedId,
        endorsers.map(x => (x.blsKp.publicKey, x.balance)),
        conflict
      )
    ) shouldBe true
    r
  }

  extension (s: EndorsementStorage) {
    def tryAddEndorsement(msg: BlockEndorsement.Full): Either[String, Boolean] = s.tryAdd(EndorseBlock.from(msg))
  }
}
