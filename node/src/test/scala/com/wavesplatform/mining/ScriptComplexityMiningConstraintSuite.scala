package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.{Blockchain, LeaseBalance}
import com.wavesplatform.transaction.{DataTransaction, Transaction}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ScriptComplexityMiningConstraintSuite
    extends FlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with PathMockFactory
    with TransactionGen
    with NoShrink {
  val settings = WavesSettings.fromRootConfig(ConfigFactory.load())

  val complexity = OneDimensionalMiningConstraint(1000, TxEstimators.scriptsComplexity, "MaxScriptsComplexityInBlock")
  val maxTxs     = OneDimensionalMiningConstraint(3, TxEstimators.one, "MaxTxsInMicroBlock")
  val constraint = MultiDimensionalMiningConstraint(complexity, maxTxs)

  "ScriptComplexityMiningConstraint" should "accept non-scripted txs after limit" in {
    forAll(preconditions) {
      case (tx1, tx2, tx3) =>
        val blockchain = stub[Blockchain]
        (blockchain.settings _).when().returning(settings.blockchainSettings)
        (blockchain.height _).when().returning(1)
        (blockchain.activatedFeatures _).when().returning(Map(BlockchainFeatures.DataTransaction.id -> 0))

        val txDiffer =
          TransactionDiffer(Some(System.currentTimeMillis() - 1000), System.currentTimeMillis(), 2)(blockchain, _: Transaction).resultE.explicitGet()
        (blockchain.balance _).when(*, *).returning(10000000)
        (blockchain.leaseBalance _).when(*).returning(LeaseBalance(0, 0))
        (blockchain.accountScriptWithComplexity _).when(tx1.sender.toAddress).returning(Some(null.asInstanceOf[Script] -> 1000))
        (blockchain.accountScriptWithComplexity _).when(*).returning(None)

        val c1          = constraint.put(blockchain, tx1, txDiffer(tx1))
        val cOverfilled = c1.put(blockchain, tx1, txDiffer(tx1))
        cOverfilled shouldBe 'overfilled

        val c2 = c1.put(blockchain, tx2, txDiffer(tx2))
        c2 should not be 'full

        val c3 = c2.put(blockchain, tx3, txDiffer(tx3))
        c3 shouldBe 'full
        c3 should not be 'overfilled
    }

  }

  private[this] def preconditions: Gen[(DataTransaction, DataTransaction, DataTransaction)] =
    for {
      acc1 <- accountGen
      acc2 <- accountGen
      tx1 = DataTransaction.selfSigned(acc1, Nil, 1000000, System.currentTimeMillis()).explicitGet()
      tx2 = DataTransaction.selfSigned(acc2, Nil, 1000000, System.currentTimeMillis()).explicitGet()
      tx3 = DataTransaction.selfSigned(acc2, Nil, 1000000, System.currentTimeMillis()).explicitGet()
    } yield (tx1, tx2, tx3)
}
