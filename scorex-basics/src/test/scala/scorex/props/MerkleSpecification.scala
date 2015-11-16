package scorex.props

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.Sha256
import scorex.crypto.ads.merkle.MerkleTree

import scala.util.Random

class MerkleSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("value returned from byIndex() is valid for random dataset") {
    val dataSetGen = for {
      dataSet <- Gen.nonEmptyContainerOf[Array, Array[Byte]](Arbitrary.arbitrary[Array[Byte]])
    } yield dataSet

    forAll(dataSetGen) { dataSet =>
      if (dataSet.length > 1) {
        val index = Random.nextInt(dataSet.length)

        val tree = MerkleTree.create(dataSet)

        val leaf = tree.byIndex(index).get
        MerkleTree.check(index, tree.hash, leaf)(Sha256)
      }
    }
  }

}