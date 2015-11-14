package scorex.perma.merkle

import scala.annotation.tailrec
import scala.math

case class AuthDataBlock[A](data: A, merklePath: Seq[MerkleTree.Digest])

//bottom up

trait MerkleTreeI[A] {

  def byIndex(n: Int): Option[AuthDataBlock[A]]
}


object MerkleTree {

  type Block = Array[Byte]
  type Digest = Vector[Byte]

  class MerkleTree[A, Hash <: CryptographicHash](val tree: Tree[A, Hash], val leaves: Seq[Tree[A, Hash]]) extends MerkleTreeI[A] {

    val Size = leaves.size
    lazy val rootNode:Node[A, Hash] = tree.asInstanceOf[Node[A, Hash]]

    def byIndex(n: Int): Option[AuthDataBlock[A]] = {
      @tailrec
      def calculateTreePath(n: Int, node:Node[A, Hash], levelSize: Int, acc: Seq[Digest] = Seq()): Seq[Digest] = {
        if(n< levelSize/2) {
          node.leftChild match {
            case leftNode:Node[A, Hash] =>
              calculateTreePath(n, leftNode, levelSize/2, node.hash +: acc)
            case _ =>
              acc
          }
        } else {
          node.rightChild match {
            case rightNode:Node[A, Hash] =>
              calculateTreePath(n - levelSize/2, rightNode, levelSize/2, node.hash +: acc)
            case _ =>
              acc
          }
        }
      }

      val leaf = leaves.lift(n)
      if (leaf.isEmpty) {
        None
      } else {
        leaf.get match {
          case Leaf(data) =>
            val treePath = calculateTreePath(n, rootNode, Size)
            Some(AuthDataBlock(data, treePath))
          case _ =>
            None
        }
      }
    }
  }


  sealed trait Tree[+A, Hash <: CryptographicHash] {
    val hash: Vector[Byte]
  }

  case class Node[+A, Hash <: CryptographicHash](
                                                  leftChild: Tree[A, Hash],
                                                  rightChild: Tree[A, Hash])(hashFunction: Hash)
    extends Tree[A, Hash] {

    override val hash: Digest =
      hashFunction.hash(leftChild.hash ++ rightChild.hash)
  }

  case class Leaf[+A, Hash <: CryptographicHash](data: A)(hashFunction: Hash)
    extends Tree[A, Hash] {

    override val hash: Digest = hashFunction.hash(data.toString.getBytes)
  }

  case class EmptyLeaf[Hash <: CryptographicHash]()(hashFunction: Hash) extends Tree[Nothing, Hash] {
    override val hash: Digest = Vector.empty[Byte]
  }

  def create[A, Hash <: CryptographicHash](
                                            dataBlocks: Seq[A],
                                            hashFunction: Hash = HashImpl): MerkleTree[A, Hash] = {
    val level = calculateRequiredLevel(dataBlocks.size)

    val dataLeaves = dataBlocks.map(data => Leaf(data)(hashFunction))

    val paddingNeeded = math.pow(2, level).toInt - dataBlocks.size
    val padding = Seq.fill(paddingNeeded)(EmptyLeaf()(hashFunction))

    val leaves: Seq[Tree[A, Hash]] = dataLeaves ++ padding

    new MerkleTree(makeTree(leaves, hashFunction), leaves)
  }

  def merge[A, Hash <: CryptographicHash](
                                           leftChild: Tree[A, Hash],
                                           rightChild: Tree[A, Hash],
                                           hashFunction: Hash): Node[A, Hash] = {
    Node(leftChild, rightChild)(hashFunction)
  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  private def calculateRequiredLevel(numberOfDataBlocks: Int): Int = {

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
