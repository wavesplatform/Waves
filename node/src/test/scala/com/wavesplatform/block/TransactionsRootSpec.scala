package com.wavesplatform.block

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.merkle.Merkle
import com.wavesplatform.block.merkle.Merkle._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.{BlockGen, NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TransactionsRootSpec
    extends FreeSpec
    with OptionValues
    with ScalaCheckPropertyChecks
    with BlockGen
    with TransactionGen
    with NoShrink
    with Matchers {
  import TransactionsRootSpec._

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

      val merkleProof2 = block.transactionProof(transaction)

      merkleProof2 shouldBe 'defined
      block.verifyTransactionProof(merkleProof2.value) shouldBe true
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

object TransactionsRootSpec {
  implicit class BlockMerkleOps(block: Block) {

    def transactionProof(transaction: Transaction): Option[TransactionProof] =
      Merkle.calcTransactionProof(block.transactionData, transaction)

    def verifyTransactionProof(transactionProof: TransactionProof): Boolean =
      block.transactionData
        .lift(transactionProof.transactionIndex)
        .filter(tx => tx.id() == transactionProof.id)
        .exists(tx => Merkle.verifyTransactionProof(transactionProof, tx, block.transactionData.size, block.header.transactionsRoot.arr))
  }
}
