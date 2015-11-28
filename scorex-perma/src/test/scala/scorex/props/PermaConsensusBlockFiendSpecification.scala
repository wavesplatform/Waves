package scorex.props

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.perma.Storage.AuthDataStorage
import scorex.perma.consensus._
import scorex.perma.settings.Constants.DataSegment
import scorex.perma.settings.PermaSettings
import scorex.settings.Settings
import scorex.storage.Storage

class PermaConsensusBlockFiendSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  implicit val settings = new Settings with PermaSettings {
    val filename = "settings-test.json"
  }
  implicit val authDataStorage: Storage[Long, AuthDataBlock[DataSegment]] = new AuthDataStorage(settings.authDataStorage)
  val consensus = new PermaConsensusModule("test".getBytes)

  property("set value and get it") {
    forAll { (diff: Long, puz: Array[Byte], pubkey: Array[Byte], s: Array[Byte], signature: Array[Byte], segmentIndex: Long) =>

      val blockdata = "nonempty".getBytes ++ puz

      val authDataBlock: AuthDataBlock[DataSegment] = AuthDataBlock(blockdata, Seq(puz, pubkey))
      val initialBlock = PermaConsensusBlockField(PermaLikeConsensusBlockData(
        math.abs(diff),
        puz,
        Ticket(pubkey, s, IndexedSeq(PartialProof(signature, segmentIndex, authDataBlock)))
      ))
      val parsedBlock = PermaConsensusBlockField.parse(initialBlock.bytes)

      checkAll(initialBlock, parsedBlock)

    }

    val initialBlock = consensus.genesisData
    val parsedBlock = PermaConsensusBlockField.parse(initialBlock.bytes)
    checkAll(initialBlock, parsedBlock)
  }

  def checkAll(initialBlock: PermaConsensusBlockField, parsedBlock: PermaConsensusBlockField): Unit = {
    parsedBlock.value.target shouldBe initialBlock.value.target
    assert(parsedBlock.value.puz sameElements initialBlock.value.puz)
    assert(parsedBlock.value.ticket.publicKey sameElements initialBlock.value.ticket.publicKey)
    assert(parsedBlock.value.ticket.s sameElements initialBlock.value.ticket.s)
    parsedBlock.value.ticket.proofs.size shouldBe initialBlock.value.ticket.proofs.size
    if(parsedBlock.value.ticket.proofs.nonEmpty) {
      assert(parsedBlock.value.ticket.proofs.head.signature sameElements initialBlock.value.ticket.proofs.head.signature)
      assert(parsedBlock.value.ticket.proofs.head.segmentIndex == initialBlock.value.ticket.proofs.head.segmentIndex)
      assert(parsedBlock.value.ticket.proofs.head.segment.data sameElements initialBlock.value.ticket.proofs.head.segment.data)
    }
  }
}