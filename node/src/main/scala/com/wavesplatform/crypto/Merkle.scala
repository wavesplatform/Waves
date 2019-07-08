package com.wavesplatform.crypto

import java.nio.ByteBuffer

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.transaction.Transaction
import play.api.libs.json.Writes
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.hash.{CryptographicHash32, Digest, Digest32}
import supertagged.TaggedType

object Merkle {

  val EMPTY_ROOT_HASH: Digest32 = Digest32 @@ Array.emptyByteArray

  object FastHash extends CryptographicHash32 {
    override def hash(input: FastHash.Message): Digest32 = Digest32 @@ com.wavesplatform.crypto.fastHash(input)
  }

  object TransactionProof extends TaggedType[MerkleProof[Digest32]]
  type TransactionProof = TransactionProof.Type

  object TransactionTree extends TaggedType[MerkleTree[Digest32]]
  type TransactionTree = TransactionTree.Type

  def mkTransactionTree(txs: Seq[Transaction]): TransactionTree = {

    val elements =
      txs.view
        .map(_.bytes())
        .map(LeafData @@ _)
        .force

    TransactionTree @@ MerkleTree[Digest32](elements)(FastHash)
  }

  implicit class TransactionTreeOps(tree: TransactionTree) {
    def getProofForTransaction(tx: Transaction): Option[TransactionProof] = {
      val el = Leaf(LeafData @@ tx.bytes())(FastHash)

      tree
        .proofByElement(el)
        .map(TransactionProof @@ _)
    }

    def transactionProofValid(proof: TransactionProof): Boolean = {
      proof.valid(tree.rootHash)
    }
  }

  object BalanceProof extends TaggedType[MerkleProof[Digest32]]
  type BalanceProof = BalanceProof.Type

  object BalanceTree extends TaggedType[MerkleTree[Digest32]]
  type BalanceTree = BalanceTree.Type

  def mkBalanceTree(bs: Seq[(Address, Long)]): BalanceTree = {
    val elements =
      bs.view
        .map(balanceToBytes)
        .map(LeafData @@ _)
        .force

    BalanceTree @@ MerkleTree[Digest32](elements)(FastHash)
  }

  implicit class MinerBalanceTreeOps(tree: BalanceTree) {
    def getProofForBalance(in: (Address, Long)): Option[BalanceProof] = {
      val el = Leaf(LeafData @@ balanceToBytes(in))(FastHash)

      tree
        .proofByElement(el)
        .map(BalanceProof @@ _)
    }

    def balanceProofValid(proof: BalanceProof): Boolean = {
      proof.valid(tree.rootHash)
    }
  }

  private def balanceToBytes(in: (Address, Long)): Array[Byte] = {
    val (addr, balance) = in
    ByteBuffer
      .allocate(Address.AddressLength + 8)
      .put(addr.bytes.arr)
      .putLong(balance)
      .array()
  }

  implicit val balanceProofWrites: Writes[BalanceProof] =
    implicitly[Writes[String]]
      .contramap[MerkleProof[Digest32]] { proof =>
        Base64.encode(proofBytes(proof))
      }

  implicit val transactionProofWrites: Writes[TransactionProof] =
    implicitly[Writes[String]]
      .contramap[MerkleProof[Digest32]] { proof =>
        Base64.encode(proofBytes(proof))
      }

  private def proofBytes(mp: MerkleProof[Digest32]): Array[Byte] = {
    def loop(lvls: List[(Digest, Side)], acc: Array[Byte]): Array[Byte] = {
      lvls match {
        case (d, s) :: xs => loop(xs, Array.concat(acc, s +: d.length.toByte +: d))
        case Nil          => acc
      }
    }

    loop(mp.levels.toList, Array.emptyByteArray)
  }

}
