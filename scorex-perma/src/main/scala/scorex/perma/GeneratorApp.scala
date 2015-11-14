package scorex.perma

import scorex.perma.merkle.MerkleTree

import scala.util.Random

object GeneratorApp extends App {

  val TREE_SIZE = 8
  val SEGMENT_SIZE = 8

  val rnd = new Random()
  val dataSet = (1 to TREE_SIZE).map(x => Random.alphanumeric.take(SEGMENT_SIZE).mkString).toArray
  val fullFile  = dataSet.mkString("")

  val tree = MerkleTree.create(dataSet)
  val hash = tree.tree.hash

  println(dataSet)
  println(fullFile)
  println(hash)
  println(tree.tree)
  println(tree.byIndex(0))
}
