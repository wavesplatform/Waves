package com.wavesplatform.utils

import java.nio.ByteBuffer

import com.wavesplatform.account.Address
import com.wavesplatform.transaction.Transaction
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.hash.{CryptographicHash32, Digest32}
import supertagged._

object Merkle {

  val EMPTY_ROOT_HASH: Digest32 = Digest32 @@ Array.emptyByteArray

  object FastHash extends CryptographicHash32 {
    override def hash(input: FastHash.Message): Digest32 = Digest32 @@ com.wavesplatform.crypto.fastHash(input)
  }

  object TxProof extends TaggedType[MerkleProof[Digest32]]
  type TxProof = TxProof.Type

  object TxTree extends TaggedType[MerkleTree[Digest32]]
  type TxTree = TxTree.Type

  def mkTxTree(txs: Seq[Transaction]): TxTree = {
    val elements =
      txs.view
        .map(_.bytes())
        .map(LeafData @@ _)
        .force

    TxTree @@ MerkleTree[Digest32](elements)(FastHash)
  }

  implicit class TxTreeOps(tree: TxTree) {
    def getProofForTx(tx: Transaction): Option[TxProof] = {
      val el = Leaf(LeafData @@ tx.bytes())(FastHash)

      tree
        .proofByElement(el)
        .map(TxProof @@ _)
    }

    def txProofValid(proof: TxProof): Boolean = {
      proof.valid(tree.rootHash)
    }
  }

  object MinerBalanceProof extends TaggedType[MerkleProof[Digest32]]
  type MinerBalanceProof = MinerBalanceProof.Type

  object MinerBalanceTree extends TaggedType[MerkleTree[Digest32]]
  type MinerBalanceTree = MinerBalanceTree.Type

  def mkMinerBalanceTree(bs: Seq[(Address, Long)]): MinerBalanceTree = {
    val elements =
      bs.view
        .map(balanceToBytes)
        .map(LeafData @@ _)
        .force

    MinerBalanceTree @@ MerkleTree[Digest32](elements)(FastHash)
  }

  implicit class MinerBalanceTreeOps(tree: MinerBalanceTree) {
    def getProofForMinerBalance(in: (Address, Long)): Option[MinerBalanceProof] = {
      val el = Leaf(LeafData @@ balanceToBytes(in))(FastHash)

      tree
        .proofByElement(el)
        .map(MinerBalanceProof @@ _)
    }

    def minerBalanceProofValid(proof: TxProof): Boolean = {
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
}
