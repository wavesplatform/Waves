package com.wavesplatform.block

import com.wavesplatform.account.KeyPair
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.{BlockGen, NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.crypto.authds.merkle.{Leaf, MerkleProof}
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
      val merkleTree = block.merkleTree()
      val proof      = merkleTree.proofByElement(Leaf(transaction.merkleLeaf())(FastHash))

      proof shouldBe 'defined
      proof.value.valid(merkleTree.rootHash) shouldBe true
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
      val merkleTree = block.merkleTree()

      val proof = merkleTree.proofByElement(Leaf(transaction.merkleLeaf())(FastHash))

      proof shouldBe 'empty
  }

  val incorrectProofScenario: Gen[(Block, MerkleProof[Digest32])] =
    for {
      (block, _)                  <- happyPathScenario
      (anotherBlock, transaction) <- happyPathScenario
      proof = anotherBlock.merkleTree().proofByElement(Leaf(transaction.merkleLeaf())(FastHash)).get
    } yield (block, proof)

  "Merkle tree for block should invalidate incorrect proof" in forAll(incorrectProofScenario) {
    case (block, proof) =>
      val merkleTree = block.merkleTree()

      proof.valid(merkleTree.rootHash) shouldBe false
  }
}
