package com.wavesplatform.block

import com.wavesplatform.account.KeyPair
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.{BlockGen, NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.hash.Digest32

class BlockMerkleSpec extends FreeSpec with OptionValues with ScalaCheckPropertyChecks with BlockGen with TransactionGen with NoShrink with Matchers {
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
      block.merkleRootValid() shouldBe true

      val merkleTree  = block.merkleTree()
      val merkleProof = block.merkleProof(transaction)

      merkleProof shouldBe 'defined
      merkleProof.value.valid(merkleTree.rootHash) shouldBe true
      merkleProof.value.valid(Digest32 @@ block.header.merkle.arr) shouldBe true
  }

  val emptyTxsDataScenario: Gen[(Block, TransferTransaction)] =
    for {
      (signer, txs) <- commonGen
      tx            <- Gen.oneOf(txs)
      block         <- versionedBlockGen(Seq.empty, signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for empty block should ignore any transaction" in forAll(emptyTxsDataScenario) {
    case (block, transaction) =>
      block.merkleRootValid() shouldBe true
      block.merkleProof(transaction) shouldBe 'empty
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
      block.merkleRootValid() shouldBe true
      block.merkleProof(transaction) shouldBe 'empty
  }

  val incorrectProofScenario: Gen[(Block, Transaction, MerkleProof[Digest32])] =
    for {
      (block, _)                  <- happyPathScenario
      (anotherBlock, transaction) <- happyPathScenario
      proof = anotherBlock.merkleProof(transaction).get
    } yield (block, transaction, proof)

  "Merkle tree for block should invalidate incorrect proof" in forAll(incorrectProofScenario) {
    case (block, transaction, proof) =>
      block.merkleRootValid() shouldBe true

      val merkleTree = block.merkleTree()

      block.merkleProof(transaction) shouldBe 'empty
      proof.valid(merkleTree.rootHash) shouldBe false
      proof.valid(Digest32 @@ block.header.merkle.arr) shouldBe false
  }
}
