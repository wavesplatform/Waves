package scorex.props

import java.io.{File, FileInputStream, FileOutputStream}

import org.scalacheck.{Gen, Arbitrary}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.Sha256
import scorex.crypto.Sha256._
import scorex.crypto.ads.merkle.MerkleTree

import scala.util.Random

class MerkleSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("value returned from byIndex() is valid for random dataset") {



    val smallInteger = Gen.choose(1, 200)

    forAll(smallInteger) { (blocks: Int) =>
      val treeDirName = "/tmp/scorex/test/" + Random.alphanumeric(8).toString
      val treeDir = new File(treeDirName)
      val tempFile = treeDirName + "/data.file"


      val data = new Array[Byte](1024 * blocks)
      Random.nextBytes(data)
      treeDir.mkdirs()
      val fos = new FileOutputStream(tempFile)
      fos.write(data)
      fos.close()
      val file = new FileInputStream(tempFile)

      val tree = MerkleTree.fromFile(file, treeDirName)
      val index = Random.nextInt(tree.nonEmptyBlocks)

      val leaf = tree.byIndex(index).get
      leaf.check(index, tree.rootHash)(Sha256)
      tree.storage.close()
      for (file <- treeDir.listFiles) file.delete
    }

  }
}