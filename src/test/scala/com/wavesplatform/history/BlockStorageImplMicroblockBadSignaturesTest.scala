package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, PaymentTransaction, TransactionParser}

class BlockStorageImplMicroblockBadSignaturesTest extends PropSpec with PropertyChecks
  with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment2: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, payment, payment2)

  property("bad total resulting block signature") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2))
      val badSigMicro = microblocks1.head.copy(totalResBlockSig = randomSig)
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(badSigMicro) should produce("InvalidSignature")
    }
  }

  property("bad microBlock signature") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2))
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
      domain.blockchainUpdater.processMicroBlock(badSigMicro) should produce("sender")
    }
  }
}
