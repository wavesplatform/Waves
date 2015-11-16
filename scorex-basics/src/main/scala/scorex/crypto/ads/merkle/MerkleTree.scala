package scorex.crypto.ads.merkle

import scorex.crypto.CryptographicHash.Digest
import scorex.crypto.{CryptographicHash, Sha256}

import scala.annotation.tailrec
import scala.math

/**
  * @param data - data block
  * @param merklePath - merkle path, complementary to data block
  */
case class AuthDataBlock[Block](data: Block, merklePath: Seq[Digest])

//bottom up

trait MerkleTreeI[Block] {
  def byIndex(n: Int): Option[AuthDataBlock[Block]]
}

//todo: check/optimize the code

object MerkleTree {

  def check[Block, Hash <: CryptographicHash](index: Int, rootHash: Digest, block: AuthDataBlock[Block])
                                             (hashFunction: Hash = Sha256): Boolean = {

    @tailrec
    def calculateHash(i: Int, nodeHash: Digest, path: Seq[Digest]): Digest = {
      if (i % 2 == 0) {
        val hash = hashFunction.hash(nodeHash ++ path.head)
        if (path.size == 1) {
          hash
        } else {
          calculateHash(i / 2, hash, path.tail)
        }
      } else {
        val hash = hashFunction.hash(path.head ++ nodeHash)
        if (path.size == 1) {
          hash
        } else {
          calculateHash(i / 2, hash, path.tail)
        }
      }
    }
    val calculated = calculateHash(index, hashFunction.hash(block.data.toString.getBytes), block.merklePath)
    calculated.mkString == rootHash.mkString
  }

  class MerkleTree[Block, Hash <: CryptographicHash](val tree: Tree[Block, Hash], val leaves: Seq[Tree[Block, Hash]])
    extends MerkleTreeI[Block] {

    val Size = leaves.size
    lazy val rootNode: Node[Block, Hash] = tree.asInstanceOf[Node[Block, Hash]]
    lazy val hash = tree.hash

    def byIndex(index: Int): Option[AuthDataBlock[Block]] = {
      @tailrec
      def calculateTreePath(n: Int, node: Node[Block, Hash], levelSize: Int, acc: Seq[Digest] = Seq()): Seq[Digest] = {
        val halfLevelSize: Int = levelSize / 2
        if (n < halfLevelSize) {
          node.leftChild match {
            case nd: Node[Block, Hash] =>
              calculateTreePath(n, nd, halfLevelSize, node.rightChild.hash +: acc)
            case _ =>
              node.rightChild.hash +: acc
          }
        } else {
          node.rightChild match {
            case nd: Node[Block, Hash] =>
              calculateTreePath(n - halfLevelSize, nd, halfLevelSize, node.leftChild.hash +: acc)
            case _ =>
              node.leftChild.hash +: acc
          }
        }
      }

      leaves.lift(index).flatMap(l =>
        l match {
          case Leaf(data: Block) =>
            val treePath = calculateTreePath(index, rootNode, Size)
            Some(AuthDataBlock(data, treePath))
          case _ =>
            None
        }
      )
    }

    override def toString: String = {
      def printHashes(node: Tree[Any, Hash], prefix: String = ""): List[String] = {
        node match {
          case Node(leftChild: Tree[Block, Hash], rightChild: Tree[Block, Hash]) =>
            (prefix + node.hash.mkString) :: printHashes(leftChild, " " + prefix) ++
              printHashes(rightChild, " " + prefix)
          case l: Leaf[Block, Hash] =>
            List(prefix + l.hash.mkString)
          case _ =>
            List()
        }
      }
      printHashes(tree).mkString("\n")
    }
  }


  sealed trait Tree[+Block, Hash <: CryptographicHash] {
    val hash: Digest
  }

  case class Node[+Block, Hash <: CryptographicHash](
                                                      leftChild: Tree[Block, Hash],
                                                      rightChild: Tree[Block, Hash])(hashFunction: Hash)
    extends Tree[Block, Hash] {

    override val hash: Digest = hashFunction.hash(leftChild.hash ++ rightChild.hash)

  }

  case class Leaf[+Block, Hash <: CryptographicHash](data: Block)(hashFunction: Hash)
    extends Tree[Block, Hash] {

    override val hash: Digest = hashFunction.hash(data.toString.getBytes)
  }

  case class EmptyLeaf[Block <: CryptographicHash]()(hashFunction: Block) extends Tree[Nothing, Block] {
    override val hash: Digest = Array.empty[Byte]
  }

  def create[Block, Hash <: CryptographicHash](
                                                dataBlocks: Seq[Block],
                                                hashFunction: Hash = Sha256): MerkleTree[Block, Hash] = {
    val level = calculateRequiredLevel(dataBlocks.size)

    val dataLeaves = dataBlocks.map(data => Leaf(data)(hashFunction))

    val paddingNeeded = math.pow(2, level).toInt - dataBlocks.size
    val padding = Seq.fill(paddingNeeded)(EmptyLeaf()(hashFunction))

    val leaves: Seq[Tree[Block, Hash]] = dataLeaves ++ padding

    new MerkleTree(makeTree(leaves, hashFunction), leaves)
  }

  def merge[Block, Hash <: CryptographicHash](
                                               leftChild: Tree[Block, Hash],
                                               rightChild: Tree[Block, Hash],
                                               hashFunction: Hash): Node[Block, Hash] = {
    Node(leftChild, rightChild)(hashFunction)
  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  private def calculateRequiredLevel(numberOfDataBlocks: Int): Int = {

    math.ceil(log2(numberOfDataBlocks)).toInt
  }

  @tailrec
  private def makeTree[Block, Hash <: CryptographicHash](
                                                          trees: Seq[Tree[Block, Hash]],
                                                          hashFunction: Hash): Tree[Block, Hash] = {
    def createParent(treePair: Seq[Tree[Block, Hash]]): Node[Block, Hash] = {
      val leftChild +: rightChild +: _ = treePair
      merge(leftChild, rightChild, hashFunction)
    }

    if (trees.isEmpty) {
      EmptyLeaf()(hashFunction)
    } else if (trees.size == 1) {
      trees.head
    } else {
      makeTree(trees.grouped(2).map(createParent).toSeq, hashFunction)
    }
  }
}