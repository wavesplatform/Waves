package scorex.perma.consensus

import java.io.{File, RandomAccessFile}

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.ads.merkle.{AuthDataBlock, MerkleTree}
import scorex.crypto.hash.FastCryptographicHash
import scorex.perma.settings.PermaConstants.DataSegment
import scorex.perma.settings.{PermaConstants, PermaSettings}
import scorex.perma.storage.AuthDataStorage
import scorex.settings.Settings
import scorex.storage.Storage
import scorex.utils._

class PermaConsensusModuleSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
with Matchers with ScorexLogging {

  implicit val settings = new Settings with PermaSettings {
    override lazy val filename = "settings-test.json"
  }

  log.info("Generating random data set")
  val treeDir = new File(settings.treeDir)
  treeDir.mkdirs()
  val datasetFile = settings.treeDir + "/data.file"
  new RandomAccessFile(datasetFile, "rw").setLength(PermaConstants.n * PermaConstants.segmentSize)
  log.info("Calculate tree")
  val tree = MerkleTree.fromFile(datasetFile, settings.treeDir, PermaConstants.segmentSize, FastCryptographicHash)
  require(tree.nonEmptyBlocks == PermaConstants.n, s"${tree.nonEmptyBlocks} == ${PermaConstants.n}")

  log.info("Test tree")
  val index = PermaConstants.n - 3
  val leaf = tree.byIndex(index).get
  require(leaf.check(index, tree.rootHash)(FastCryptographicHash))

  log.info("Put ALL data to local storage")
  new File(settings.treeDir).mkdirs()
  implicit lazy val authDataStorage: Storage[Long, AuthDataBlock[DataSegment]] = new AuthDataStorage(settings.authDataStorage)

  def addBlock(i: Long): Unit = {
    authDataStorage.set(i, tree.byIndex(i).get)
    if (i > 0) {
      addBlock(i - 1)
    }
  }

  addBlock(PermaConstants.n - 1)

  val rootHash = tree.rootHash
  val consensus = new PermaConsensusModule(rootHash)

  property("generate/validate roundtrip") {
    forAll { (seed: Array[Byte], puz: Array[Byte], wrongBytes: Array[Byte]) =>
      whenever(seed.nonEmpty && puz.nonEmpty && wrongBytes.nonEmpty && !wrongBytes.sameElements(puz)) {
        val keyPair = EllipticCurveImpl.createKeyPair(seed)
        val ticket = consensus.generate(keyPair, puz).get
        val publicKey = keyPair._2
        consensus.validate(publicKey, puz, consensus.ticketScore(ticket) + 1, ticket, rootHash) shouldBe true
        consensus.validate(publicKey, puz, consensus.ticketScore(ticket), ticket, rootHash) shouldBe false
        consensus.validate(randomBytes(EllipticCurveImpl.KeyLength), puz, consensus.ticketScore(ticket) + 1, ticket, rootHash) shouldBe false
        consensus.validate(publicKey, wrongBytes, consensus.ticketScore(ticket) + 1, ticket, rootHash) shouldBe false
        consensus.validate(publicKey, puz, consensus.ticketScore(ticket) + 1, ticket, wrongBytes) shouldBe false
      }
    }
  }


}