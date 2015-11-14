package scorex.perma.merkle

import scala.annotation.tailrec
import scala.math

trait MerkleTree[Data] {

  case class AuthDataBlock(data: Data, merklePath: Seq[Digest]) //bottom up

  type Digest = Array[Byte]

  val Size: Int

  def byIndex(n: Int): AuthDataBlock // require(n < Size)
}


object MerkleTree {

  sealed trait Tree[+A, Hash <: CryptographicHash] {
    val hash: Vector[Byte]
  }

  case class Node[+A, Hash <: CryptographicHash](
                                                  leftChild: Tree[A, Hash],
                                                  rightChild: Tree[A, Hash])(hashFunction: Hash)
    extends Tree[A, Hash] {

    override val hash: Vector[Byte] =
      hashFunction.hash(leftChild.hash ++ rightChild.hash)
  }

  case class Leaf[+A, Hash <: CryptographicHash](data: A)(hashFunction: Hash)
    extends Tree[A, Hash] {

    override val hash: Vector[Byte] = hashFunction.hash(data.toString.getBytes)
  }

  case class EmptyLeaf[Hash <: CryptographicHash]()(hashFunction: Hash) extends Tree[Nothing, Hash] {
    override val hash: Vector[Byte] = Vector.empty[Byte]
  }

  def create[A, Hash <: CryptographicHash](
                                            dataBlocks: Seq[A],
                                            hashFunction: Hash = HashImpl): Tree[A, Hash] = {
    val level = calculateRequiredLevel(dataBlocks.size)

    val dataLeaves = dataBlocks.map(data => Leaf(data)(hashFunction))

    val paddingNeeded = math.pow(2, level).toInt - dataBlocks.size
    val padding = Seq.fill(paddingNeeded)(EmptyLeaf()(hashFunction))

    val leaves = dataLeaves ++ padding

    makeTree(leaves, hashFunction)
  }

  def merge[A, Hash <: CryptographicHash](
                                           leftChild: Tree[A, Hash],
                                           rightChild: Tree[A, Hash],
                                           hashFunction: Hash): Node[A, Hash] = {
    Node(leftChild, rightChild)(hashFunction)
  }

  private def calculateRequiredLevel(numberOfDataBlocks: Int): Int = {
    def log2(x: Double): Double = math.log(x) / math.log(2)

    math.ceil(log2(numberOfDataBlocks)).toInt
  }

  @tailrec
  private def makeTree[A, Hash <: CryptographicHash](
                                                      trees: Seq[Tree[A, Hash]],
                                                      hashFunction: Hash): Tree[A, Hash] = {
    def createParent(treePair: Seq[Tree[A, Hash]]): Node[A, Hash] = {
      val leftChild +: rightChild +: _ = treePair
      merge(leftChild, rightChild, hashFunction)
    }

    if (trees.size == 0) {
      EmptyLeaf()(hashFunction)
    } else if (trees.size == 1) {
      trees.head
    } else {
      makeTree(trees.grouped(2).map(createParent).toSeq, hashFunction)
    }
  }
}
