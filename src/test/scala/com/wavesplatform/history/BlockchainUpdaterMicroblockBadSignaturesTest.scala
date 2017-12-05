package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{GenesisTransaction, TransactionParser}

class BlockchainUpdaterMicroblockBadSignaturesTest extends PropSpec with PropertyChecks
  with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  val preconditionsAndPayments: Gen[(GenesisTransaction, TransferTransaction, TransferTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: TransferTransaction <- wavesTransferGeneratorP(master, recipient)
    payment2: TransferTransaction <- wavesTransferGeneratorP(master, recipient)
  } yield (genesis, payment, payment2)

  property("bad total resulting block signature") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2).map(Seq(_)))
      val badSigMicro = microblocks1.head.copy(totalResBlockSig = randomSig)
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(badSigMicro) should produce("InvalidSignature")
    }
  }

  property("bad microBlock signature") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2).map(Seq(_)))
      val badSigMicro = microblocks1.head.copy(signature = randomSig)
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(badSigMicro) should produce("InvalidSignature")
    }
  }

  property("other sender") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2)) =>
      val otherSigner = PrivateKeyAccount(TestBlock.randomOfLength(TransactionParser.KeyLength).arr)
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val block1 = buildBlockOfTxs(block0.uniqueId, Seq(payment))
      val badSigMicro = buildMicroBlockOfTxs(block0.uniqueId, block1, Seq(payment2), otherSigner)._2
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(badSigMicro) should produce("another account")
    }
  }
}
