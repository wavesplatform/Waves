package scorex.perma.network

import java.io.{File, FileOutputStream}

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.ads.merkle.MerkleTree
import scorex.crypto.hash.FastCryptographicHash
import scorex.perma.settings.PermaConstants

import scala.util.Random

class SegmentsMessageSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  val segmentsMessageSp = SegmentsMessageSpec
  val getSegmentsMessageSpec = GetSegmentsMessageSpec
  val (treeDirName: String, _, tempFile: String) = generateFile(PermaConstants.n.toInt)
  val tree = MerkleTree.fromFile(tempFile, treeDirName, PermaConstants.segmentSize)

  property("SegmentsMessageSpec: Encode to bytes round-trip") {
    forAll { (in: Seq[Long]) =>
      val indexes = in.map(Math.abs).map(_ % PermaConstants.n)
      whenever(indexes.forall(i => i < PermaConstants.n)) {
        val data = indexes.map(i => i -> tree.byIndex(i).get).toMap
        val serialized = segmentsMessageSp.serializeData(data)
        data.forall(s => s._2.check(s._1, tree.rootHash)(FastCryptographicHash)) shouldBe true
        val deserealized = segmentsMessageSp.deserializeData(serialized).get
        deserealized.keySet shouldBe indexes.toSet
        indexes.foreach { i =>
          data(i).data shouldBe deserealized(i).data
          data(i).merklePath.size shouldBe deserealized(i).merklePath.size
          data(i).merklePath(0) shouldBe deserealized(i).merklePath(0)
          data(i).merklePath(1) shouldBe deserealized(i).merklePath(1)
          data(i).merklePath(2) shouldBe deserealized(i).merklePath(2)
        }
        deserealized.forall(s => s._2.check(s._1, tree.rootHash)(FastCryptographicHash)) shouldBe true
      }
    }
  }

  property("GetSegmentsMessageSpec: Encode to bytes round-trip") {
    forAll { (indexes: Seq[Long]) =>
      val serialized = getSegmentsMessageSpec.serializeData(indexes)
      val deserealized = getSegmentsMessageSpec.deserializeData(serialized).get
      indexes shouldBe deserealized
    }
  }

  def generateFile(blocks: Int, subdir: String = "1"): (String, File, String) = {
    val treeDirName = "/tmp/scorex-test/test/" + subdir + "/"
    val treeDir = new File(treeDirName)
    val tempFile = treeDirName + "/data.file"

    val data = new Array[Byte](1024 * blocks)
    Random.nextBytes(data)
    treeDir.mkdirs()
    for (file <- treeDir.listFiles) file.delete

    val fos = new FileOutputStream(tempFile)
    fos.write(data)
    fos.close()
    (treeDirName, treeDir, tempFile)
  }
}