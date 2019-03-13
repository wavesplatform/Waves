package com.wavesplatform.utils

import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.hash.{CryptographicHash, Digest32}

package object mtree {

  type MTree  = MerkleTree[Digest32]
  type MProof = MerkleProof[Digest32]

  def computeTree[A](elements: Seq[A])(
      implicit
      pr: ProvableBytes[A],
      ch: CryptographicHash[Digest32]
  ): MTree = {
    val data =
      elements.view
        .map(pr.provableBytes)
        .map(LeafData @@ _)
        .force

    MerkleTree[Digest32](data)(ch)
  }

  def getProofFor[A](el: A, tree: MTree)(
      implicit
      pr: ProvableBytes[A],
      ch: CryptographicHash[Digest32]
  ): Option[MProof] = {
    tree.proofByElement(Leaf[Digest32](LeafData @@ pr.provableBytes(el)))
  }
}
