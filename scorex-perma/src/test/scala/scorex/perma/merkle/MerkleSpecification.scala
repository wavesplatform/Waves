package scorex.perma.merkle

import org.scalatest.{Matchers, FunSuite}

import scala.util.Random

class MerkleSpecification extends FunSuite with Matchers {

  test("Should get by index and check it") {
    val TREE_SIZE = 16
    val SEGMENT_SIZE = 8
    val Index = 3

    val rnd = new Random()
//    val dataSet = (1 to TREE_SIZE).map(x => Random.alphanumeric.take(SEGMENT_SIZE).mkString).toArray
    val dataSet = Array("1234", "5678", "9101", "1213")

    val fullFile  = dataSet.mkString("")

    val tree = MerkleTree.create(dataSet)
    println(tree)
    println("---")

    val leaf = tree.byIndex(Index).get
    val data = leaf.data
    val path = leaf.merklePath
    leaf.merklePath.map { x=>
      println(x.mkString)
    }
    println("????")

    val c = MerkleTree.check(Index, tree.hash, data, path)(HashImpl)
    assert(c)
  }
}