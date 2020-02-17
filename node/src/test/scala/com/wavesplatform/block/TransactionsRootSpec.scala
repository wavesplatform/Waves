package com.wavesplatform.block

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.merkle.Merkle._
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.{BlockGen, NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.{Leaf, MerkleTree}
import scorex.crypto.hash.Digest32

class TransactionsRootSpec
    extends FreeSpec
    with OptionValues
    with ScalaCheckPropertyChecks
    with BlockGen
    with TransactionGen
    with NoShrink
    with Matchers {
  val commonGen: Gen[(KeyPair, List[TransferTransaction])] =
    for {
      signer    <- accountGen
      sender    <- accountGen
      recipient <- accountGen
      txsLength <- Gen.choose(1, 1000)
      txs       <- Gen.listOfN(txsLength, versionedTransferGeneratorP(sender, recipient, Waves, Waves))
    } yield (signer, txs)

  val happyPathScenario: Gen[(Block, Transaction)] =
    for {
      (signer, txs) <- commonGen
      tx            <- Gen.oneOf(txs)
      block         <- versionedBlockGen(txs, signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for block should validate correct transaction" in forAll(happyPathScenario) {
    case (block, transaction) =>
      block.transactionsRootValid() shouldBe true

      val merkleProof = block.transactionProof(transaction)

      merkleProof shouldBe 'defined
      block.verifyTransactionProof(merkleProof.value) shouldBe true
  }

  val emptyTxsDataScenario: Gen[(Block, TransferTransaction)] =
    for {
      (signer, txs) <- commonGen
      tx            <- Gen.oneOf(txs)
      block         <- versionedBlockGen(Seq.empty, signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for empty block should ignore any transaction" in forAll(emptyTxsDataScenario) {
    case (block, transaction) =>
      block.transactionsRootValid() shouldBe true
      block.transactionProof(transaction) shouldBe 'empty
  }

  val incorrectTransactionScenario: Gen[(Block, Transaction)] =
    for {
      (signer, txs)    <- commonGen
      anotherSender    <- accountGen
      anotherRecipient <- accountGen
      tx               <- versionedTransferGeneratorP(anotherSender, anotherRecipient, Waves, Waves)
      block            <- versionedBlockGen(txs, signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for block should ignore incorrect transaction" in forAll(incorrectTransactionScenario) {
    case (block, transaction) =>
      block.transactionsRootValid() shouldBe true
      block.transactionProof(transaction) shouldBe 'empty
  }

  val singleTransactionScenario: Gen[(Block, Transaction)] =
    for {
      (signer, txs) <- commonGen
      tx            <- Gen.oneOf(txs)
      block         <- versionedBlockGen(Seq(tx), signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for block with single transaction should validate it" in forAll(singleTransactionScenario) {
    case (block, transaction) =>
      block.transactionsRootValid() shouldBe true

      val merkleProof = block.transactionProof(transaction)

      merkleProof shouldBe 'defined
      block.verifyTransactionProof(merkleProof.value) shouldBe true
      merkleProof.value.digests.head shouldBe Array.emptyByteArray

      val nativeLeafData    = LeafData @@ PBTransactions.protobuf(transaction).toByteArray
      val nativeMerkleTree  = MerkleTree(Seq(nativeLeafData))
      val nativeMerkleProof = nativeMerkleTree.proofByElement(Leaf(nativeLeafData))
      nativeMerkleProof shouldBe 'defined
      nativeMerkleProof.value.valid(Digest32 @@ block.header.transactionsRoot.arr)

      // it's okay to have empty digest in a single level for the tree with the only one leaf
      nativeMerkleProof.value.levels.map(_._1.toList) shouldBe merkleProof.value.digests.map(_.toList)
      merkleProof.value.digests.nonEmpty shouldBe true
      merkleProof.value.digests.head.isEmpty shouldBe true
  }

  val incorrectProofScenario: Gen[(Block, Transaction, TransactionProof)] =
    for {
      (block, _)                  <- happyPathScenario
      (anotherBlock, transaction) <- happyPathScenario
      proof = anotherBlock.transactionProof(transaction).get
    } yield (block, transaction, proof)

  "Merkle tree for block should invalidate incorrect proof" in forAll(incorrectProofScenario) {
    case (block, transaction, proof) =>
      block.transactionsRootValid() shouldBe true
      block.transactionProof(transaction) shouldBe 'empty

      block.verifyTransactionProof(proof) shouldBe false
  }
}
