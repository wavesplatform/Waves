package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.*
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, LeaseBalance}
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{DataTransaction, Transaction, TxVersion}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory

class ScriptComplexityMiningConstraintSuite extends FlatSpec with PathMockFactory {
  private val settings = WavesSettings.fromRootConfig(ConfigFactory.load())

  private val complexity = OneDimensionalMiningConstraint(1000, TxEstimators.scriptsComplexity, "MaxScriptsComplexityInBlock")
  private val maxTxs     = OneDimensionalMiningConstraint(3, TxEstimators.one, "MaxTxsInMicroBlock")
  private val constraint = MultiDimensionalMiningConstraint(complexity, maxTxs)

  val (script, _) = ScriptCompiler.compile("true", ScriptEstimatorV3.latest).explicitGet()

  "ScriptComplexityMiningConstraint" should "accept non-scripted txs after limit" in {
    forAll(preconditions) { case (acc1, acc2, tx1, tx2, tx3) =>
      val blockchain = stub[Blockchain]
      (() => blockchain.settings).when().returning(settings.blockchainSettings)
      (() => blockchain.height).when().returning(1)
      (() => blockchain.activatedFeatures).when().returning(Map(BlockchainFeatures.DataTransaction.id -> 0))

      val txDiffer = (tx: Transaction) => {
        val time = System.currentTimeMillis()
        TransactionDiffer(Some(time - 1000), time)(blockchain, tx).resultE
          .explicitGet()
      }
      (blockchain.balance _).when(*, *).returning(10000000)
      (blockchain.wavesBalances _).when(*).returning(Map(acc1.toAddress -> 10000000, acc2.toAddress -> 10000000))
      (blockchain.leaseBalance _).when(*).returning(LeaseBalance(0, 0))
      (blockchain.accountScript _).when(tx1.sender.toAddress).returning(Some(AccountScriptInfo(acc1.publicKey, script, 1000, Map.empty)))
      (blockchain.accountScript _).when(*).returning(None)

      val c1          = constraint.put(blockchain, tx1, txDiffer(tx1))
      val cOverfilled = c1.put(blockchain, tx1, txDiffer(tx1))
      cOverfilled.isOverfilled shouldBe true

      val c2 = c1.put(blockchain, tx2, txDiffer(tx2))
      c2.isFull shouldBe false

      val c3 = c2.put(blockchain, tx3, txDiffer(tx3))
      c3.isFull shouldBe true
      c3.isOverfilled shouldBe false
    }

  }

  private[this] def preconditions: Gen[(KeyPair, KeyPair, DataTransaction, DataTransaction, DataTransaction)] =
    for {
      acc1 <- accountGen
      acc2 <- accountGen
      tx1 = DataTransaction.selfSigned(TxVersion.V1, acc1, Nil, 1000000, System.currentTimeMillis()).explicitGet()
      tx2 = DataTransaction.selfSigned(TxVersion.V1, acc2, Nil, 1000000, System.currentTimeMillis()).explicitGet()
      tx3 = DataTransaction.selfSigned(TxVersion.V1, acc2, Nil, 1000000, System.currentTimeMillis()).explicitGet()
    } yield (acc1, acc2, tx1, tx2, tx3)
}
