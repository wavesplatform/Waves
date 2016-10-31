package scorex.network

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.consensus.ConsensusModule
import scorex.crypto.EllipticCurveImpl
import scorex.network.message.BasicMessagesRepo
import scorex.transaction.TransactionModule

class CheckpointSpecification extends FreeSpec
  with Matchers
  with MockFactory
  with OneInstancePerTest {

  private val maxRollback = 10

  "history points" in {
    val h = 100000
    Checkpoint.historyPoints(h, maxRollback, 3) shouldBe Seq(h - 10, h - 100, h - 1000)

    Checkpoint.historyPoints(2, maxRollback, 3) shouldBe Seq()

    Checkpoint.historyPoints(1000, maxRollback, 3) shouldBe Seq(990, 900)

    Checkpoint.historyPoints(h, maxRollback, 2) shouldBe Seq(h - 10, h - 100)

    println(Checkpoint.historyPoints(198500, 100, 100).toList)
  }

  "serialization" in {
    val spec = new BasicMessagesRepo()(stub[TransactionModule[Unit]], stub[ConsensusModule[Unit]]).CheckpointMessageSpec

    def sig(b: Byte) = Array.fill[Byte](EllipticCurveImpl.SignatureLength)(b)

    val c = Checkpoint(Seq(BlockCheckpoint(1, sig(1)), BlockCheckpoint(2, sig(2))), sig(3))

    val c2 = spec.deserializeData(spec.serializeData(c)).get

    c2.items should have size 2
    (c2.signature sameElements c.signature) shouldBe true
  }
}
