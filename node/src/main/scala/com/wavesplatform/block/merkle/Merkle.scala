package com.wavesplatform.block.merkle

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Transaction
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.hash.{CryptographicHash32, Digest32}

import scala.annotation.tailrec

object Merkle {

  private[block] implicit object FastHash extends CryptographicHash32 { // todo: (NODE-1972) Replace with appropriate hash function
    override def hash(input: Message): Digest32 = Digest32 @@ com.wavesplatform.crypto.fastHash(input)
  }

  case class TransactionProof(
      id: ByteStr,
      transactionIndex: Int,
      digests: Seq[Array[Byte]]
  )

  /** Calculates transaction's root */
  def calcTransactionRoot(transactionData: Seq[Transaction]): ByteStr =
    ByteStr(mkMerkleTree(transactionData).rootHash)

  /** Calculates transaction's proof */
  def calcTransactionProof(
      block: Block,
      transaction: Transaction
  ): Option[TransactionProof] =
    for {
      proof               <- block.transactionsMerkleTree().proofByElement(mkMerkleLeaf(transaction))
      (_, transactionIdx) <- block.transactionData.zipWithIndex.find { case (tx, _) => tx.id() == transaction.id() }
    } yield TransactionProof(transaction.id(), transactionIdx, proof.levels.map(_._1))

  /** Verifies transaction's proof */
  def verifyTransactionProof(
      transactionProof: TransactionProof,
      transaction: Transaction,
      transactionsCount: Int,
      transactionsRoot: Array[Byte]
  ): Boolean = {
    val valid = for {
      sides <- sides(transactionsCount, transactionProof.transactionIndex)
      leaf  = mkMerkleLeaf(transaction)
      proof = MerkleProof(leaf.data, transactionProof.digests.zip(sides).map { case (d, s) => (Digest32 @@ d, s) })
    } yield proof.valid(Digest32 @@ transactionsRoot)
    valid.getOrElse(false)
  }

  private val EmptyMerkleTree: MerkleTree[Digest32] = MerkleTree(Seq(LeafData @@ Array.emptyByteArray))

  /** Creates transactions merkle root */
  private[block] def mkMerkleTree(transactions: Seq[Transaction]): MerkleTree[Digest32] = {
    if (transactions.isEmpty) EmptyMerkleTree else MerkleTree(transactions.map(mkMerkleLeaf(_).data))
  }

  private[block] def mkMerkleLeaf(transaction: Transaction): Leaf[Digest32] =
    Leaf(LeafData @@ PBTransactions.protobuf(transaction).toByteArray)

  private sealed trait SideNode                                 extends Product with Serializable
  private case class SideInternalNode(l: SideNode, r: SideNode) extends SideNode
  private case object SideEmptyNode                             extends SideNode
  private case object SideLeafNode                              extends SideNode

  private def sides(leafsSize: Int, leafIndex: Int): Option[Seq[Side]] = {
    def log2(x: Double): Double = math.log(x) / math.log(2)

    @tailrec
    def calcNode(nodes: Seq[SideNode]): SideInternalNode = {
      val next = nodes.grouped(2).map(lr => SideInternalNode(lr.head, if (lr.length == 2) lr.last else SideEmptyNode)).toSeq
      if (next.length == 1) next.head else calcNode(next)
    }

    @tailrec
    def mkSides(node: SideNode, i: Int, cur: Int, acc: Seq[Side]): Option[Seq[Side]] =
      node match {
        case n: SideInternalNode if i < cur / 2 => mkSides(n.l, i, cur / 2, acc :+ MerkleProof.LeftSide)
        case n: SideInternalNode if i < cur     => mkSides(n.r, i - cur / 2, cur / 2, acc :+ MerkleProof.RightSide)
        case SideLeafNode                       => Some(acc.reverse)
        case _                                  => None
      }

    val node   = calcNode((0 until leafsSize).map(_ => SideLeafNode))
    val height = Math.max(math.pow(2, math.ceil(log2(leafsSize))).toInt, 2)
    mkSides(node, leafIndex, height, Seq())
  }
}
