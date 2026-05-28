package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.{AccountScriptInfo, Height}
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{Asset, DataTransaction, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.EmptyBlockchain
import org.scalacheck.Gen

class ScriptComplexityMiningConstraintSuite extends FlatSpec {
  private val defaultSettings = WavesSettings.fromRootConfig(ConfigFactory.load())

  private val complexity = OneDimensionalMiningConstraint(1000, TxEstimators.scriptsComplexity, "MaxScriptsComplexityInBlock")
  private val maxTxs     = OneDimensionalMiningConstraint(3, TxEstimators.one, "MaxTxsInMicroBlock")
  private val constraint = MultiDimensionalMiningConstraint(complexity, maxTxs)

  val (script, _) = ScriptCompiler.compile("true", ScriptEstimatorV3.latest).explicitGet()

  "ScriptComplexityMiningConstraint" should "accept non-scripted txs after limit" in {
    forAll(preconditions) { case (acc1, acc2, tx1, tx2, tx3) =>
      val blockchain = new EmptyBlockchain {
        override lazy val settings: BlockchainSettings                          = defaultSettings.blockchainSettings
        override def height: Int                                                = 1
        override def activatedFeatures: Map[Short, Height]                      = Map(BlockchainFeatures.DataTransaction.id -> Height(0))
        override def wavesBalances(addresses: Seq[Address]): Map[Address, Long] = Map(acc1.toAddress -> 10000000, acc2.toAddress -> 10000000)
        override def balance(address: Address, mayBeAssetId: Asset): Long       = 10000000
        override def accountScript(address: Address): Option[AccountScriptInfo] =
          if (address == tx1.sender.toAddress) Some(AccountScriptInfo(acc1.publicKey, script, 1000, Map.empty)) else None
      }

      val txDiffer = (tx: Transaction) => {
        val time = System.currentTimeMillis()
        TransactionDiffer(Some(time - 1000), time)(blockchain, tx).resultE
          .explicitGet()
      }

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

  private def preconditions: Gen[(KeyPair, KeyPair, DataTransaction, DataTransaction, DataTransaction)] =
    for {
      acc1 <- accountGen
      acc2 <- accountGen
      tx1 = TxHelpers.data(account = acc1, entries = Nil, fee = 1000000, version = TxVersion.V1)
      tx2 = TxHelpers.data(account = acc2, entries = Nil, fee = 1000000, version = TxVersion.V1)
      tx3 = TxHelpers.data(account = acc2, entries = Nil, fee = 1000000, version = TxVersion.V1)
    } yield (acc1, acc2, tx1, tx2, tx3)
}
