package scorex.props

import java.io.{File, FileInputStream, FileOutputStream}

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.Sha256
import scorex.crypto.ads.merkle.MerkleTree

import scala.util.Random

class MerkleSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {
  val smallInteger = Gen.choose(100, 500)


  property("value returned from byIndex() is valid for random dataset") {
    forAll(smallInteger) { (blocks: Int) =>
      val (treeDirName: String, treeDir: File, tempFile: String) = generateFile(blocks)
      val file = new FileInputStream(tempFile)

      val tree = MerkleTree.fromFile(file, treeDirName)
      val index = Random.nextInt(tree.nonEmptyBlocks)

      val leaf = tree.byIndex(index).get
      val resp = leaf.check(index, tree.rootHash)(Sha256)
      tree.storage.close()
      resp shouldBe true
    }
  }

  property("hash root is the same") {
    forAll(smallInteger) { (blocks: Int) =>
      val (treeDirName: String, treeDir: File, tempFile: String) = generateFile(blocks, "2")
      val file = new FileInputStream(tempFile)

      val fileTree = MerkleTree.fromFile(file, treeDirName)
      val rootHash = fileTree.rootHash

      fileTree.storage.close()

      val tree = new MerkleTree(treeDirName, fileTree.nonEmptyBlocks)
      val newRootHash = tree.rootHash
      tree.storage.close()
      rootHash shouldBe newRootHash
    }
  }


  def generateFile(blocks: Int, subdir: String = "1"): (String, File, String) = {
    val treeDirName = "/tmp/scorex/test/" + subdir + "/"
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