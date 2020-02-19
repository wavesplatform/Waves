package com.wavesplatform.block.merkle

import java.util

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Transaction
import io.estatico.newtype.macros.newtype
import scorex.crypto.hash.Blake2b256

import scala.annotation.tailrec

object Merkle {
  @newtype private case class Digest(toBytes: Array[Byte]) {
    def concat(that: Digest): Message = Message(toBytes ++ that.toBytes)
    def eq(that: Digest): Boolean     = util.Arrays.equals(toBytes, that.toBytes)
  }

  @newtype private case class Message(toBytes: Array[Byte])

  private trait HashFunction {
    def hash(input: Message): Digest
  }

  private implicit val hf: HashFunction = (input: Message) => Digest(Blake2b256.hash(input.toBytes))

  private sealed trait Node extends Product with Serializable {
    def hash: Digest
  }

  private case class InternalNode(l: Node, r: Node)(implicit hf: HashFunction) extends Node {
    override lazy val hash: Digest = hf.hash(l.hash concat r.hash)
  }

  private case class LeafNode(data: Message)(implicit hf: HashFunction) extends Node {
    override lazy val hash: Digest = hf.hash(data)
  }

  private case class EmptyNode()(implicit hf: HashFunction) extends Node {
    override def hash: Digest = hf.hash(Message(Array(0.toByte)))
  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  @tailrec
  private def traverseProofPath[A](node: Node, i: Int, curLength: Int, acc: Seq[A], leftOp: Node => A, rightOp: Node => A): Option[Seq[A]] = {
    node match {
      case InternalNode(l, r) if i < curLength / 2 => traverseProofPath(l, i, curLength / 2, leftOp(r) +: acc, leftOp, rightOp)
      case InternalNode(l, r) if i < curLength     => traverseProofPath(r, i - curLength / 2, curLength / 2, rightOp(l) +: acc, leftOp, rightOp)
      case LeafNode(_)                             => Some(acc)
      case _                                       => None
    }
  }

  @tailrec
  private def calcTopNode(nodes: Seq[Node])(implicit hf: HashFunction): Node = {
    val nextNodes = nodes
      .grouped(2)
      .map {
        case Seq(l, r) => InternalNode(l, r)
        case Seq(l)    => InternalNode(l, EmptyNode())
      }
      .toSeq
    if (nextNodes.length == 1) nextNodes.head else calcTopNode(nextNodes)
  }

  case class TransactionProof(id: ByteStr, transactionIndex: Int, digests: Seq[Array[Byte]]) {
    import TransactionProof._

    def valid(transaction: Transaction, transactionsCount: Int, transactionsRoot: ByteStr): Boolean =
      (for {
        sides <- sides(transactionsCount, transactionIndex)
        leafDigest = LeafNode(Message(PBTransactions.protobuf(transaction).toByteArray)).hash
        levels     = digests.zip(sides).map { case (d, s) => (Digest(d), s) }
      } yield {
        levels
          .foldLeft(leafDigest) {
            case (prevDigest, (digest, Left))  => hf.hash(prevDigest concat digest)
            case (prevDigest, (digest, Right)) => hf.hash(digest concat prevDigest)
          }
          .toBytes
          .sameElements(transactionsRoot.arr)
      }).getOrElse(false)

  }

  object TransactionProof {
    private val noOp: HashFunction = _ => Digest(Array.emptyByteArray)

    private sealed trait Side extends Product with Serializable
    private case object Left  extends Side
    private case object Right extends Side

    private def sides(leafsSize: Int, leafIndex: Int): Option[Seq[Side]] = {
      val leaf    = LeafNode(Message(Array.emptyByteArray))(noOp)
      val topNode = calcTopNode((0 until leafsSize).map(_ => leaf))(noOp)
      val height  = Math.max(math.pow(2, math.ceil(log2(leafsSize))).toInt, 2)
      traverseProofPath[Side](topNode, leafIndex, height, Seq(), _ => Left, _ => Right)
    }
  }

  class TransactionsTree private (topNode: Node, digestIndexes: Map[ByteStr, (Digest, Int)]) {
    lazy val transactionsRoot: ByteStr = ByteStr(topNode.hash.toBytes)
    lazy val length: Int               = digestIndexes.size
    lazy val lengthWithEmptyLeafs: Int = Math.max(math.pow(2, math.ceil(log2(length))).toInt, 2)

    def transactionProof(tx: Transaction): Option[TransactionProof] = {
      val digest = LeafNode(Message(PBTransactions.protobuf(tx).toByteArray)).hash
      digestIndexes.get(tx.id()).collect { case (d, i) if d eq digest => i }.flatMap { transactionIndex =>
        traverseProofPath(topNode, transactionIndex, lengthWithEmptyLeafs, Seq(), _.hash, _.hash)
          .map(digests => TransactionProof(tx.id(), transactionIndex, digests.map(_.toBytes)))
      }
    }
  }

  object TransactionsTree {
    def apply(transactions: Seq[Transaction]): TransactionsTree =
      if (transactions.isEmpty) new TransactionsTree(EmptyNode(), Map())
      else {
        val leafsWithIds  = transactions.map(tx => (tx.id(), LeafNode(Message(PBTransactions.protobuf(tx).toByteArray))))
        val leafs         = leafsWithIds.map(_._2)
        val digestIndexes = leafsWithIds.zipWithIndex.map { case ((id, leaf), index) => (id, (leaf.hash, index)) }.toMap

        val topNode = calcTopNode(leafs)
        new TransactionsTree(topNode, digestIndexes)
      }
  }
}
