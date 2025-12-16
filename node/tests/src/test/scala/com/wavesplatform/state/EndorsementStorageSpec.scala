package com.wavesplatform.state

import cats.syntax.traverse.*
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.network.EndorseBlock
import com.wavesplatform.state.{EndorsementFilter, EndorsementStorage, GeneratorIndex, Height}
import com.wavesplatform.test.{FreeSpec, NumericExt, produce}
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position
import org.scalatest.EitherValues

class EndorsementStorageSpec extends FreeSpec with EitherValues {
  private type GeneratorBalance = (addr: Address, blsKp: BlsKeyPair, balance: Long)

  private val committedGenerator = BlsKeyPair(TxHelpers.signer(0).privateKey) // GeneratorIndex(0)

  private val activeGenerator      = BlsKeyPair(TxHelpers.signer(1).privateKey)
  private val activeGeneratorIndex = GeneratorIndex(1)

  private val expectedFinalizedHeight = Height(5)

  private val expectedFinalizedId, unexpectedFinalizedId, expectedEndorsedId = TxHelpers.randomBlockId

  private def mkGenerators(n: Int): IndexedSeq[GeneratorBalance] = (0 until n).map { i =>
    val wavesKp = TxHelpers.signer(i)
    val blsKp   = BlsKeyPair(wavesKp.privateKey)
    (wavesKp.toAddress, blsKp, 100_000.waves)
  }

  private val defaultGenerators: IndexedSeq[GeneratorBalance] = mkGenerators(4)

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
      started(minerIndex = 1).tryAddEndorsement(mk()).value shouldBe false
    }

    "ignore if" - {
      "an endorsement with" - {
        def test(msg: EndorseBlock, error: String): Unit = started().tryAdd(msg) should produce(error)

        "a wrong signature" in test(
          EndorseBlock(activeGeneratorIndex.toInt, expectedFinalizedId, expectedFinalizedHeight, expectedEndorsedId, ByteStr.empty),
          "Invalid signature"
        )

        "an unexpected finalized height" in test(
          EndorseBlock.from(mk(finalizedHeight = expectedFinalizedHeight.next)),
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
    "returns None" - {
      "if not reached finalization" in {
        val s = started(minerIndex = 3, defaultGenerators)

        log.info("no endorsements")
        s.checkTryCollect(expectedEndorsedId)

        log.info("after endorsement #0")
        s.addValidVote(0) // 0 and miner
        s.checkTryCollect(expectedEndorsedId)
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

        "and lost finalization because of conflict votes" in {
          val s = started(minerIndex = 3, defaultGenerators)

          log.debug("reached finalization because of valid votes")
          s.addValidVote(0, 1)
          s.checkTryCollect(expectedEndorsedId, Seq(0, 1))

          log.debug("lost finalization, removes from valid")
          s.addConflictVote(0, 1)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(0, 1))
        }
      }

      "when got a new conflict vote" - {
        "even no valid votes" in {
          val s = started(minerIndex = 3, defaultGenerators)

          s.addConflictVote(2)
          s.checkTryCollect(expectedEndorsedId, conflict = Seq(2))
        }

        "even insufficient valid votes" in {
          val s = started(minerIndex = 3, mkGenerators(5))

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
      minerIndex: Int = -1,
      generators: IndexedSeq[GeneratorBalance] = mkGenerators(2),
      conflict: Set[GeneratorIndex] = Set.empty,
      hasSameBlockBeforeFinalizationHeight: Boolean = true
  ): ExtendedEndorsementStorage = {
    require(minerIndex == -1 || minerIndex >= 0 && minerIndex < generators.size, s"Invalid miner index $minerIndex")
    val r = new EndorsementStorage.InMemory((_, _) => hasSameBlockBeforeFinalizationHeight)
    r.startVoting(
      EndorsementFilter(
        GeneratorIndex.checked(minerIndex),
        expectedFinalizedId,
        expectedFinalizedHeight,
        expectedEndorsedId,
        generators.map(x => (x.addr, x.blsKp.publicKey, x.balance)),
        conflict
      )
    ) shouldBe true
    new ExtendedEndorsementStorage(r, generators)
  }

  class ExtendedEndorsementStorage(inner: EndorsementStorage, generators: IndexedSeq[GeneratorBalance]) {
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
}
