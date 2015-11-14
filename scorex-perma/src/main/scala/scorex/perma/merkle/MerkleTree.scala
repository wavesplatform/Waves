package scorex.perma.merkle

import scorex.perma.merkle.CryptographicHash.Digest

import scala.annotation.tailrec
import scala.math

case class AuthDataBlock[A](data: A, merklePath: Seq[Digest])

//bottom up

trait MerkleTreeI[A] {

  def byIndex(n: Int): Option[AuthDataBlock[A]]
}


object MerkleTree {

  def check[A, Hash <: CryptographicHash](index: Int, rootHash: Digest, data: A, treePath: Seq[Digest])
                                         (hashFunction: Hash = HashImpl): Boolean = {

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
    val calculated = calculateHash(index, hashFunction.hash(data.toString.getBytes), treePath)
    calculated.mkString == rootHash.mkString
  }

  class MerkleTree[A, Hash <: CryptographicHash](val tree: Tree[A, Hash], val leaves: Seq[Tree[A, Hash]])
    extends MerkleTreeI[A] {

    val Size = leaves.size
    lazy val rootNode: Node[A, Hash] = tree.asInstanceOf[Node[A, Hash]]
    lazy val hash = tree.hash

    def byIndex(index: Int): Option[AuthDataBlock[A]] = {
      @tailrec
      def calculateTreePath(n: Int, node: Node[A, Hash], levelSize: Int, acc: Seq[Digest] = Seq()): Seq[Digest] = {
        if (n < levelSize / 2) {
          node.leftChild match {
            case nd: Node[A, Hash] =>
              calculateTreePath(n, nd, levelSize / 2, node.rightChild.hash +: acc)
            case _ =>
              node.rightChild.hash +: acc
          }
        } else {
          node.rightChild match {
            case nd: Node[A, Hash] =>
              calculateTreePath(n - levelSize / 2, nd, levelSize / 2, node.leftChild.hash +: acc)
            case _ =>
              node.leftChild.hash +: acc
          }
        }
      }

      val leaf = leaves.lift(index)
      if (leaf.isEmpty) {
        None
      } else {
        leaf.get match {
          case Leaf(data) =>
            val treePath = calculateTreePath(index, rootNode, Size)
            Some(AuthDataBlock(data, treePath))
          case _ =>
            None
        }
      }
    }

    override def toString(): String = {
      def printHashes(node: Tree[Any, Hash], prefix: String = ""): List[String] = {
        node match {
          case Node(leftChild: Tree[A, Hash], rightChild: Tree[A, Hash]) =>
            (prefix + node.hash.mkString) :: printHashes(leftChild, " " + prefix) ++
              printHashes(rightChild, " " + prefix)
          case l: Leaf[A, Hash] =>
            List(prefix + l.hash.mkString)
          case _ =>
            List()
        }
      }
      printHashes(tree).mkString("\n")
    }
  }


  sealed trait Tree[+A, Hash <: CryptographicHash] {
    val hash: Digest
  }

  case class Node[+A, Hash <: CryptographicHash](
                                                  leftChild: Tree[A, Hash],
                                                  rightChild: Tree[A, Hash])(hashFunction: Hash)
    extends Tree[A, Hash] {

    override val hash: Digest = hashFunction.hash(leftChild.hash ++ rightChild.hash)

  }

  case class Leaf[+A, Hash <: CryptographicHash](data: A)(hashFunction: Hash)
    extends Tree[A, Hash] {

    override val hash: Digest = hashFunction.hash(data.toString.getBytes)
  }

  case class EmptyLeaf[Hash <: CryptographicHash]()(hashFunction: Hash) extends Tree[Nothing, Hash] {
    override val hash: Digest = Array.empty[Byte]
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

    if (trees.isEmpty) {
      EmptyLeaf()(hashFunction)
    } else if (trees.size == 1) {
      trees.head
    } else {
      makeTree(trees.grouped(2).map(createParent).toSeq, hashFunction)
    }
  }
}
