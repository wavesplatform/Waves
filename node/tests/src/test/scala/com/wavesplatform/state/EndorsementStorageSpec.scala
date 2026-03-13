package com.wavesplatform.state

import cats.syntax.traverse.*
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.network.EndorseBlock
import com.wavesplatform.test.{FreeSpec, NumericExt, produce}
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position
import org.scalatest.EitherValues

class EndorsementStorageSpec extends FreeSpec with EitherValues {
  private type TestGenerator = (addr: Address, blsKp: BlsKeyPair, balance: Long)

  private val committedGenerator = BlsKeyPair(TxHelpers.signer(0).privateKey) // GeneratorIndex(0)

  private val activeGenerator      = BlsKeyPair(TxHelpers.signer(1).privateKey)
  private val activeGeneratorIndex = GeneratorIndex(1)

  private val expectedFinalizedHeight = Height(5)

  private val expectedFinalizedId, unexpectedFinalizedId, expectedEndorsedId = TxHelpers.randomBlockId

  private def mkGeneratorSet(n: Int): IndexedSeq[TestGenerator] = (0 until n).map(mkGenerator(_, 100_000.waves))

  private def mkPoorGenerator(i: Int): TestGenerator = mkGenerator(i, 0L)

  private def mkGenerator(i: Int, initBalance: Long): TestGenerator = {
    val wavesKp = TxHelpers.signer(i)
    val blsKp   = BlsKeyPair(wavesKp.privateKey)
    (wavesKp.toAddress, blsKp, initBalance)
  }

  private val defaultGenerators: IndexedSeq[TestGenerator] = mkGeneratorSet(4)

  "tryAddVote" - {
    def mk(
        endorserAccount: BlsKeyPair = activeGenerator,
        endorserIndex: GeneratorIndex = activeGeneratorIndex,
        finalizedId: BlockId = expectedFinalizedId,
        finalizedHeight: Height = expectedFinalizedHeight,
        endorsedId: BlockId = expectedEndorsedId
    ): BlockEndorsement = BlockEndorsement.signed(endorserAccount, endorserIndex, finalizedId, finalizedHeight, endorsedId)

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
            .tryAddEndorsement(mk(finalizedId = unexpectedFinalizedId, finalizedHeight = expectedFinalizedHeight - 1))
            .value shouldBe true
        }
      }
    }

    "don't rebroadcast if miner" in {
      started(isMiner = true).tryAddEndorsement(mk()).value shouldBe false
    }

    "ignore if" - {
      "an endorsement with" - {
        def test(msg: EndorseBlock, error: String): Unit = started().tryAdd(msg) should produce(error)

        "a wrong signature" in test(
          EndorseBlock(activeGeneratorIndex.toInt, expectedFinalizedId, expectedFinalizedHeight, expectedEndorsedId, ByteStr.empty),
          "Unexpected BLS signature length: 0, expected 96"
        )

        "an unexpected finalized height" in test(
          EndorseBlock.from(mk(finalizedHeight = expectedFinalizedHeight.next)),
          "Expected finalized height"
        )

        "an invalid finalized height" in test(
          EndorseBlock.from(mk(finalizedHeight = Height(0))),
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

      "generator set is empty from beginning of period" in {
        started(normalizedGeneratorSet = Vector.empty).tryAddEndorsement(mk()) should produce("Voting hasn't started")
      }

      "not enough generator balance" in {
        started(
          normalizedGeneratorSet = Vector(
            mkGenerator(0, 100_000.waves),
            mkPoorGenerator(1) // activeGenerator
          )
        ).tryAddEndorsement(mk()) should produce("has no enough balance") // activeGenerator
      }
    }
  }

  "tryCollectAndClear" - {
    "returns None" - {
      "if not reached finalization" - {
        "basic case" in {
          val s = started(minerIndex = 3, defaultGenerators)

          log.info("no endorsements")
          s.checkTryCollect(expectedEndorsedId)

          log.info("after endorsement #0")
          s.addValidVote(0) // 0 and miner
          s.checkTryCollect(expectedEndorsedId)
        }

        "reached the limit" in {
          val s = started(minerIndex = 4, mkGeneratorSet(5), maxValidEndorsers = 1)

          s.addValidVote(0 to 4*)
          s.checkTryCollect(expectedEndorsedId)
        }
      }

      "on second request if we already reached finalization even we have a new valid vote" in {
        val s = started(minerIndex = 3, defaultGenerators)

        log.info("reached")
        s.addValidVote(0, 1) // 0, 1 and miner, reached finalization
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
          val s = started(minerIndex = 3, defaultGenerators)

          s.addValidVote(2)
          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(2))
        }

        "conflict, then valid" in {
          val s = started(minerIndex = 3, defaultGenerators)

          s.addConflictVote(2)
          s.addValidVote(2)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(2))
        }
      }

      "when reached finalization" - {
        "because of conflict vote" in {
          val s = started(minerIndex = 3, defaultGenerators)

          log.debug("conflict votes reduce required balance to finalization")
          s.addConflictVote(1, 2)

          log.debug("reached finalization")
          s.addValidVote(0) // and 3
          s.checkTryCollect(expectedEndorsedId, valid = Seq(0), conflict = Seq(1, 2))
        }

        "then lost finalization because of conflict votes, then reached again" in {
          val s = started(
            minerIndex = 1,
            normalizedGeneratorSet = Vector(
              mkGenerator(0, 5000.waves),
              mkGenerator(1, 2000.waves),
              mkGenerator(2, 3000.waves)
            )
          )

          log.debug("reached finalization because of valid vote")
          s.addValidVote(0)
          s.checkTryCollect(expectedEndorsedId, Seq(0))

          log.debug("lost finalization, removes from valid")
          s.addConflictVote(0)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(0))

          log.debug("reached again")
          s.addValidVote(2)
          s.checkTryCollect(expectedEndorsedId, valid = Seq(2)) // No new conflict endorsements
        }
      }

      "when got a new conflict vote" - {
        "even no valid votes" in {
          val s = started(minerIndex = 3, defaultGenerators)

          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(2))
        }

        "even insufficient valid votes" in {
          val s = started(minerIndex = 3, mkGeneratorSet(5))

          s.addValidVote(0)
          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(2))
        }

        "after finalization" in {
          val s = started(minerIndex = 3, defaultGenerators)

          s.addValidVote(0, 1)
          s.checkTryCollect(expectedEndorsedId, valid = Seq(0, 1))

          log.debug("after finalization")
          s.addConflictVote(2)

          // 0 and 3 enough for finalization, because generator set is: 0, 1, 3
          s.checkTryCollect(expectedEndorsedId, valid = Seq(0), conflict = Seq(2))
        }
      }
    }
  }

  private def started(
      minerIndex: Int = 0,
      normalizedGeneratorSet: IndexedSeq[TestGenerator] = mkGeneratorSet(2),
      conflict: Set[GeneratorIndex] = Set.empty,
      hasSameBlockBeforeFinalizationHeight: Boolean = true,
      isMiner: Boolean = false,
      maxValidEndorsers: Int = 2
  ): ExtendedEndorsementStorage = {
    require(normalizedGeneratorSet.isEmpty || minerIndex >= 0 && minerIndex < normalizedGeneratorSet.size, s"Invalid miner index $minerIndex")
    val r = new EndorsementStorage.InMemory((_, _) => hasSameBlockBeforeFinalizationHeight)
    r.startVoting(
      EndorsementFilter(
        maxValidEndorsers,
        GeneratorIndex(minerIndex),
        isMiner,
        expectedFinalizedId,
        expectedFinalizedHeight,
        expectedEndorsedId,
        normalizedGeneratorSet.map(x => (x.addr, x.blsKp.publicKey, x.balance)),
        conflict
      )
    ) shouldBe true
    new ExtendedEndorsementStorage(r, normalizedGeneratorSet)
  }

  class ExtendedEndorsementStorage(inner: EndorsementStorage, generators: IndexedSeq[TestGenerator]) {
    export inner.*

    def addValidVote(generatorIndexes: Int*): Either[String, Boolean]    = addVotes(expectedFinalizedId, generatorIndexes)
    def addConflictVote(generatorIndexes: Int*): Either[String, Boolean] = addVotes(unexpectedFinalizedId, generatorIndexes)

    def addVotes(finalizedId: BlockId, generatorIndexes: Seq[Int]): Either[String, Boolean] =
      generatorIndexes.traverse((generatorIndex: Int) => addVote(finalizedId, generatorIndex)).map(_.last)

    def addVote(finalizedId: BlockId, generatorIndex: Int): Either[String, Boolean] = tryAddEndorsement(
      BlockEndorsement
        .signed(generators(generatorIndex).blsKp, GeneratorIndex(generatorIndex), finalizedId, expectedFinalizedHeight, expectedEndorsedId)
    )

    def tryAddEndorsement(msg: BlockEndorsement): Either[String, Boolean] = inner.tryAdd(EndorseBlock.from(msg))

    def checkTryCollect(endorsedId: BlockId, valid: Seq[Int] = Nil, conflict: Seq[Int] = Nil)(using Position): Unit =
      inner.tryCollectAndClear(endorsedId) match {
        case None if valid.nonEmpty || conflict.nonEmpty =>
          fail(s"Expected valid endorsers [${valid.mkString(", ")}], conflict endorsers [${conflict.mkString(", ")}], got None")
        case Some(v) =>
          withClue("valid: ") {
            v.valid should contain theSameElementsAs valid
          }
          withClue("conflict: ") {
            v.conflict.map(_.endorserIndex) should contain theSameElementsAs conflict
          }
          withClue("signature and valid endorsements: ") {
            v.aggregatedEndorsement match {
              case None => if (valid.nonEmpty) fail(s"Signature can't be empty if endorsers nonempty: [${valid.mkString(", ")}]")
              case Some(aggEnd) =>
                if (valid.isEmpty) fail(s"Signature must be empty if endorsers empty: $aggEnd, [${valid.mkString(", ")}]")
                else
                  aggEnd.verifyAgg(
                    BlockEndorsement.mkMessage(expectedFinalizedId, expectedFinalizedHeight, endorsedId),
                    valid.map(generators(_).blsKp.publicKey)
                  ) should beRight
            }
          }
        case _ =>
      }
  }
}
