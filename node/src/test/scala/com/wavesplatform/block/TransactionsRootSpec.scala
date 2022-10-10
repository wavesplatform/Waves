package com.wavesplatform.block

import com.wavesplatform.BlockGen
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.merkle.Merkle.*
import com.wavesplatform.crypto.Blake2b256
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalacheck.Gen
import org.scalatest.OptionValues

class TransactionsRootSpec extends FreeSpec with OptionValues with BlockGen {

  val commonGen: Gen[(KeyPair, List[TransferTransaction])] =
    for {
      signer    <- accountGen
      sender    <- accountGen
      recipient <- accountGen
      txsLength <- Gen.choose(1, 1000)
      txs       <- Gen.listOfN(txsLength, versionedTransferGeneratorP(sender, recipient.toAddress, Waves, Waves))
    } yield (signer, txs)

  val validProofsScenario: Gen[(List[Transaction], Int)] =
    for {
      (_, txs) <- commonGen
      idx      <- Gen.choose(0, txs.size - 1)
    } yield (txs, idx)

  "Merkle should validate correct proofs" in forAll(validProofsScenario) { case (txs, idx) =>
    val messages = txs.map(PBTransactions.protobuf(_).toByteArray)
    val digest   = hash(messages(idx))

    val levels = mkLevels(messages)
    val proofs = mkProofs(idx, levels)

    verify(digest, idx, proofs, levels.head.head) shouldBe true
  }

  val invalidProofsScenario: Gen[((List[Transaction], Int), (List[Transaction], Int))] =
    for {
      (txs, idx)              <- validProofsScenario
      (anotherTx, anotherIdx) <- validProofsScenario
    } yield ((txs, idx), (anotherTx, anotherIdx))

  "Merkle should invalidate incorrect proofs" in forAll(invalidProofsScenario) { case ((txs, idx), (anotherTxs, anotherIdx)) =>
    val messages        = txs.map(PBTransactions.protobuf(_).toByteArray)
    val anotherMessages = anotherTxs.map(PBTransactions.protobuf(_).toByteArray)

    val levels        = mkLevels(messages)
    val anotherLevels = mkLevels(anotherMessages)
    val anotherProofs = mkProofs(anotherIdx, anotherLevels)
    val anotherDigest = hash(anotherMessages(anotherIdx))

    verify(anotherDigest, idx, anotherProofs, levels.head.head) shouldBe false
  }

  val happyPathScenario: Gen[(Block, Transaction)] =
    for {
      (signer, txs) <- commonGen
      tx            <- Gen.oneOf(txs)
      block         <- versionedBlockGen(txs, signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for block should validate correct transaction" in forAll(happyPathScenario) { case (block, transaction) =>
    block.signatureValid() shouldBe true

    val merkleProof = block.transactionProof(transaction)

    block.verifyTransactionProof(merkleProof.value) shouldBe true
  }

  val emptyTxsDataScenario: Gen[(Block, TransferTransaction)] =
    for {
      (signer, txs) <- commonGen
      tx            <- Gen.oneOf(txs)
      block         <- versionedBlockGen(Seq.empty, signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for empty block should ignore any transaction" in forAll(emptyTxsDataScenario) { case (block, transaction) =>
    block.signatureValid() shouldBe true
    block.transactionProof(transaction) shouldBe None
    block.header.transactionsRoot.arr should contain theSameElementsAs Blake2b256.hash(Array(0.toByte))
  }

  val incorrectTransactionScenario: Gen[(Block, Transaction)] =
    for {
      (signer, txs)    <- commonGen
      anotherSender    <- accountGen
      anotherRecipient <- accountGen
      tx               <- versionedTransferGeneratorP(anotherSender, anotherRecipient.toAddress, Waves, Waves)
      block            <- versionedBlockGen(txs, signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for block should ignore incorrect transaction" in forAll(incorrectTransactionScenario) { case (block, transaction) =>
    block.signatureValid() shouldBe true
    block.transactionProof(transaction) shouldBe None
  }

  val singleTransactionScenario: Gen[(Block, Transaction)] =
    for {
      (signer, txs) <- commonGen
      tx            <- Gen.oneOf(txs)
      block         <- versionedBlockGen(Seq(tx), signer, Block.ProtoBlockVersion)
    } yield (block, tx)

  "Merkle tree for block with single transaction should validate it" in forAll(singleTransactionScenario) { case (block, transaction) =>
    block.signatureValid() shouldBe true

    val merkleProof = block.transactionProof(transaction)

    block.verifyTransactionProof(merkleProof.value) shouldBe true
    merkleProof.value.digests.head shouldBe Blake2b256.hash(Array(0.toByte))
  }

  val incorrectProofScenario: Gen[(Block, Transaction, TransactionProof)] =
    for {
      (block, _)                  <- happyPathScenario
      (anotherBlock, transaction) <- happyPathScenario
      proof = anotherBlock.transactionProof(transaction).get
    } yield (block, transaction, proof)

  "Merkle tree for block should invalidate incorrect proof" in forAll(incorrectProofScenario) { case (block, transaction, proof) =>
    block.signatureValid() shouldBe true
    block.transactionProof(transaction) shouldBe None

    block.verifyTransactionProof(proof) shouldBe false
  }
}
